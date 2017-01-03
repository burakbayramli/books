/* Chi-square - CRD - Complete randomized design - contingency table - testing proportions - independence*/

/* Does parental usage of alcohol/marijuana influence student usage?
The following was taken from the paper "Marijuana Use in College 
(Youth and Society, 1979, 323-34). Four hundred and forty five college
students were classified according to both frequency of marijuana use and
parental use of alcohol and psychoactive drugs. */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='marijuana-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


title 'Is student usage of alcohol/marijuana independent of parental usage?';

*---part001b;
data marijuana;
   infile 'marijuana.csv' dlm=',' dsd missover firstobs=2;
   length parentuse $12. studentuse $12.;
   input parentuse $ studentuse $ count;
run;
*---part001e;


proc print data=marijuana;
   title2 'raw data';
run;



*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=marijuana;
   by parentuse;
run;
proc means data=marijuana noprint;
   by parentuse;
   var count;
   output out=totalcount sum=totalcount;
run;
data marijuana;
   merge marijuana totalcount;
   by parentuse;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=marijuana;
   title2 'percents computed';
run;
*---part020e;


ods document name=chart1(write);
*---part021b;
proc sgplot data=marijuana;
   title2 'Side-by-side segmented barcharts';
   vbar parentuse / group=studentuse 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;



ods document name=freq1(write);
*---part030b;
ods graphics on;
proc freq data=marijuana;
   title2 'Contingency table analysis';
   table parentuse * studentuse / nocol nopercent chisq 
         plot=all;
   weight count;
   ods output ChiSq=chisq1;
   ods output CrossTabFreqs=freq1;
run;
ods graphics off;
*---part030e;
ods document close;


ods pdf close;





/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=chart1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='marijuana-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=marijuana;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='marijuana-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='marijuana-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='marijuana-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;



