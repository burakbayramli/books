/* Chi-square - CRD - Complete randomized design - contingency table - testing proportions - independence*/

/* Accidents along the Trans-Canada highway were cross-classified
   by the cause of the accident and by the outcome of the accident.  */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='accident-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Is the outcome (alive/dead) independent of the cause of the accident?';

*---part001b;
data accident;
   length cause $12. result $12.;
   infile 'accident.csv' dlm=',' dsd missover firstobs=2;
   input cause $ result $ count;
run;
*---part001e;

proc print data=accident;
   title2 'raw data';
run;

*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=accident;
   by cause;
run;
proc means data=accident noprint;
   by cause;
   var count;
   output out=totalcount sum=totalcount;
run;
data accident;
   merge accident totalcount;
   by cause;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=accident;
   title2 'percents computed';
run;
*---part020e;


ods document name=chart1(write);
*---part021b;
proc sgplot data=accident;
   title2 'Side-by-side segmented barcharts';
   vbar cause / group=result 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;



ods document name=freq1(write);
*---part030b;
ods graphics on;
proc freq data=accident;
   title2 'Contingency table analysis';
   table cause * result / nocol nopercent chisq 
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

ods tagsets.mycolorlatex file='accident-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=accident;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='accident-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='accident-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='accident-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;


