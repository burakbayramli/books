/* Chi-square - CRD - Complete randomized design - contingency table - testing proportions - independence*/

/* Does the type of ownership of a natural resource (e.g.\ oyster leases)  affect the long term
prospects? A sample of oyster leases was classified by type of ownership
and the long-term prospects   */



/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='oyster-lease-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


title 'Is ownership of an oyster lease independent of outlook?';

*---part001b;
data leases;
   infile 'oyster-lease.csv' dlm=',' dsd missover firstobs=2;
   length ownership $12. outlook $12.;
   input ownership $ outlook $ count;
run;
*---part001e;


proc print data=leases;
   title2 'raw data';
run;



*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=leases;
   by ownership;
run;
proc means data=leases noprint;
   by ownership;
   var count;
   output out=totalcount sum=totalcount;
run;
data leases;
   merge leases totalcount;
   by ownership;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=leases;
   title2 'percents computed';
run;
*---part020e;


ods document name=chart1(write);
*---part021b;
proc sgplot data=leases;
   title2 'Side-by-side segmented barcharts';
   vbar ownership / group=outlook 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;



ods document name=freq1(write);
*---part030b;
ods graphics on;
proc freq data=leases;
   title2 'Contingency table analysis';
   table ownership * outlook / nocol nopercent chisq 
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

ods tagsets.mycolorlatex file='oyster-lease-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=leases;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='oyster-lease-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='oyster-lease-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='oyster-lease-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;



