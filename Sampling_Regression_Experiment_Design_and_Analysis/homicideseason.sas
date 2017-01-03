/* Chi-square test; exactl proportions known */

/* Is there a relationship between weather and violent crime? In
   the paper `Is there a season for homicide' (Criminology,
   1988), the author classified 1361 homicides by season. */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='homicideseason-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Homicide by season';

*---part001b;
data homicide;
   length season $20.;
   infile 'homicideseason.csv' dlm=',' dsd missover firstobs=2;  
   input season $   HomCount;
run;
*---part001e;

proc print data=homicide;
   title2 'raw data';
 run;

ods document name=freq1(write);
*---part010b;
ods graphics on;
proc freq data=homicide ;
   title2 'compute actual proportions and test';
   /* the testp specifies the proportions in order of season classes (alphabetical) */
   table season / nocum testp=(.25 .25 .25 .25) chisq cl all plots=all;
   table season / binomial(level=1);
   table season / binomial(level=2);
   table season / binomial(level=3);
   table season / binomial(level=4);
   weight HomCount;
   ods output OneWayChiSq=chisq1;
   ods output OneWayFreqs=freq1;
   ods output BinomialProp=binprop1;
run;
ods graphics off;
*---part010e;
ods document close;


ods pdf close;





/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=freq1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='homicideseason-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=homicide;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='homicideseason-SAS-010-devplot' reset=index;
proc document name=freq1;
   replay \Freq#1\Table1#1\DeviationPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='homicideseason-SAS-010-chitest.tex' (notop nobot);
proc print data=chisq1 noobs label split=" ";
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='homicideseason-SAS-010-freqs.tex' (notop nobot);
proc print data=freq1(obs=4) noobs label split=" " ;
   var season frequency percent testpercent;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='homicideseason-SAS-010-binprop.tex' (notop nobot);
proc print data=binprop1 noobs label split=" ";
   where name1 in ("_BIN_", "E_BIN", "L_BIN", "U_BIN");
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;

