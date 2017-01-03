/* Chi-square - CRD - Complete randomized design - contingency table - testing proportions - independence*/

/* 
A survey was conducted on feeding behavior of sand pipers at two sites close to Vancouver, B.C.
The two sites are Boundary Bay (BB) and Sidney Island (SI).

An observer went to each site and observed flocks of sand pipers. The
observer recorded the time spent by a flock between landing (most often to feed)
and departing (either besite of predator nearby or other cues).

The protocol called for the observer to stop recording and to move to a new
flock if the time spent exceeded 10 minutes. This is a form of censoring - the actual
time the flock spent on the ground is unknown; all that is known in these cases is
if the time exceeded 10 minutes.

Had the exact times been recorded for every flock, the analysis would be
straightforward. It would analyzed as a single factor (site with 2 levels)
with a completely randomized design. Of course, the inference is
limited to these two sites and we are making assumptions that once flocks
leave, their subsequent return, reformation, and activity may be
treated as introducing a ``new, independent'' flock.

Sophisticated methods are available to deal with the censored data, but a simple
analysis can be conducted by classifying the time spent into three categories: 0-5 minutes, 5-10 minutes,
and 10+ minutes. [The division into 5 minute time blocks is somewhat arbitrary].  */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='flocks-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Is the time spent feeding independent of site?';

*---part001b;
data flocks;
   infile 'flocks.csv' dlm=',' dsd missover firstobs=2;
   length site $12. timeclass $12.;
   input site $ time;
   if   0 <= time < 300 then timeclass = '00-05 minutes';
   if 300 <= time < 600 then timeclass = '05-10 minutes';
   if 600 <= time       then timeclass = '10+ minutes';
   count = 1;
run;
*---part001e;



proc print data=flocks;
   title2 'raw data';
run;


*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=flocks;
   by site;
run;
proc means data=flocks noprint;
   by site;
   var count;
   output out=totalcount sum=totalcount;
run;
data flocks;
   merge flocks totalcount;
   by site;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
*---part020e;
*proc print data=flocks;
*   title2 'percents computed';
*run;


ods document name=chart1(write);
*---part021b;
proc sgplot data=flocks;
   title2 'Side-by-side segmented barcharts';
   vbar site / group=timeclass 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---part021e;
ods document close;


ods document name=freq1(write);
*---part030b;
ods graphics on;
proc freq data=flocks;
   title2 'Contingency table analysis';
   table site * timeclass / nocol nopercent chisq
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

ods tagsets.mycolorlatex file='flocks-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=flocks(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='flocks-SAS-020-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='flocks-SAS-030-chitest.tex' (notop nobot);
proc print data=chisq1(obs=2) noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='flocks-SAS-030-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;

