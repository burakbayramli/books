/* Chi-square test two groups; contingency table;  */

/* A survey of people was selected and asked about their car preferences.
   This is the Car Poll.jmp data file from the JMP Sample Data Directory */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='carpoll-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
title 'Car Poll';

*---part001b;
data carpoll;
   infile 'carpoll.csv' firstobs=2 dlm=',' dsd missover;;
   input sex $ marital_status $ age country $ size $ type $;
   count = 1; /* number of people with these characteristics */
;;;;
*---part001e;

proc print data=carpoll (obs=20);
   title2 'part of the raw data';
run;

 
*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=carpoll;
   by sex;
run;
proc means data=carpoll noprint;
   by sex;
   var count;
   output out=totalcount sum=totalcount;
run;
data carpoll;
   merge carpoll totalcount;
   by sex;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
*---part020e;

*proc print data=carpoll; /* This file is too large to be printed here */
*   title2 'percents computed';
*run;


ods document name=chart1(write);
*---part021b;
proc sgplot data=carpoll;
   title2 'Side-by-side segmented barcharts';
   vbar sex / group=country 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;



ods document name=freq1(write);
*---part030b;
ods graphics on;
proc freq data=carpoll;
   title2 'Contingency table analysis';
   table sex * country  / nocol nopercent chisq
         plot=all;
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

ods tagsets.mycolorlatex file='carpoll-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=carpoll(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='carpoll-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='carpoll-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='carpoll-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;

