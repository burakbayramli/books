/* Chi-square test on two groups; contingency table; Fisher's Exact Test */
 
/* Example of a two-way table using fox-squirrel data on numbers predated */

/* Lines starting with *---partxxxe and *---partxxxb are used in my Latex file and should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;


title 'Predation of fox squirrels';

options noovp nodate orientation=landscape;
ods pdf file='fox-squirrel-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


*---part0001b;
data squirrel;
   length sex outcome $10.;
   input sex $ outcome $ count;
   datalines;
m predated 0
m not-pred 15
f predated 3
f not-pred 12
;;;;
*---part0001e;

proc print data=squirrel;
   title2 'raw data';
run;

ods document name=chisq1(write);
*---part0002b;
ods graphics on;
proc freq data=squirrel;
   title2 'simple two-way table analysis';
   table sex*outcome / chisq measures cl relrisk riskdiff 
        nocol nopercent expected;
   exact fisher or chisq ;
   weight count;
   ods output FishersExact=FishersExact;
run;
ods graphics off;
*---part0002e;
ods document close;

ods pdf close;





/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=chisq1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='fox-squirrel-SAS-01.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fishersexact noobs label split=' ';
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fox-squirrel-SAS-10.tex' (notop nobot);
proc document name=chisq1;
  obstitle \Freq#1\Table1#1\ChiSq#1  ;
  obtitle \Freq#1\Table1#1\ChiSq#1  ;
  replay \Freq#1\Table1#1\ChiSq#1 / levels=all;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fox-squirrel-SAS-10-expected.tex' (notop nobot);
proc document name=chisq1;
  obstitle \Freq#1\Table1#1\CrossTabFreqs#1 ;
  obtitle \Freq#1\Table1#1\CrossTabFreqs#1 ;
  replay \Freq#1\Table1#1\CrossTabFreqs#1 / levels=all;
run;
ods tagsets.mycolorlatex close;
