/* Chi-square test two groups; contingency table; resource selection */

/* Marcum and Loftsgaarden (1980)  use data from Marcum (1975) where
   200 randomly selected points were located oo a map of a study area
   which contained a mixture of forest-canopy cover classes. These
   were broken into 4 cover classes as shown below.  */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='elkhabitat-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Elk canopy-class usage';

*---part001b;
data elkhabitat;
   infile 'elkhabitat.csv' dlm=',' dsd missover firstobs=2;
   input canopy $ random elk;
   /* stack the data */
   source = 'random';
   count  =  random;
   output;
   source = 'elk';
   count  = elk;
   output;
   keep source canopy count;
run;
*---part001e;


proc print data=elkhabitat;
   title2 'raw data';
run;


*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=elkhabitat;
   by source;
run;
proc means data=elkhabitat noprint;
   by source;
   var count;
   output out=totalcount sum=totalcount;
run;
data elkhabitat;
   merge elkhabitat totalcount;
   by source;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=elkhabitat;
   title2 'percents computed';
run;
*---part020e;


ods document name=chart1(write);
*---part021b;
proc sgplot data=elkhabitat;
   title2 'Side-by-side segmented barcharts';
   vbar source / group=canopy 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;



ods document name=freq1(write);
*---part030b;
ods graphics on; 
proc freq data=elkhabitat;
   title2 'Contingency table analysis';
   table source * canopy  / nocol nopercent chisq
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

ods tagsets.mycolorlatex file='elkhabitat-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=elkhabitat;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='elkhabitat-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='elkhabitat-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='elkhabitat-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;


