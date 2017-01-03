/* Chi-square test on two groups; contingency table; */
 
/* Example of a two-way table using the data of Agresti (2002) Table 3.1
   Swedish study on asprin use and myocardinal infection */

/* Lines starting with *---partxxxe and *---partxxxb are used in my Latex file and should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

title 'Swedish study on apirin use and myocardinal infactions'; 

options nodate noovp orientation=landscape linesize=70;
ods pdf file='aspirin-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part0001b;
data aspirin;
   length aspirin mi $4.;
   input aspirin $ mi $ count;
   datalines;
yes yes 18
yes no 658
no yes 28
no no 656
;;;;
*---part0001e;


proc print data=aspirin;
   title2 'raw data';
run;

*---part0002b;
ods graphics on;
proc freq data=aspirin;
   title2 'simple two-way table analysis';
   table aspirin*mi / chisq measures cl relrisk riskdiff nocol nopercent;
   exact fisher or chisq ;
   weight count;
   ods output FishersExact=FishersExact;
run;
ods graphics off;
*---part0002e;

ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;



ods tagsets.mycolorlatex file='aspirin-SAS-01.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fishersexact noobs label split=' ';
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;





