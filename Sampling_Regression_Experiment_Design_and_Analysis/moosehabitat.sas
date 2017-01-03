/* Chi-square test; exactl proportions known; resource selection */

/* Neu et al. (1974) considered selection of habitat by Moose {\it Alces alces}
   in the Little Sioux Burn area of Minnesota in 1971-72.
   The authors determined the proportion of four habitat categories (see table below)
   using an aerial photograph, and latter classified moose usage of 117 moose  during
   later aerial surveys.  */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='moosehabitat-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Moose resource selection';

*---part001b;
data moose;
   length habitat $20.;
   infile 'moosehabitat.csv' dlm=',' dsd missover firstobs=2;  
   input habitat $  RealProp MooseCount;
   run;
*---part001e;

proc print data=moose;
   title2 'raw data';
run;

 
ods document name=freq1(write);
*---part010b;
ods graphics on;
proc freq data=moose;
   title2 'compute actual proportions and test';
   /* the testp specifies the proportions in order of habitat classes (alphabetical) */
   /* Only the pearson chi-square test statistic is available - the likelihood ratio test
      statistic cannot be obtained. Hence the test statistic (but not the p-value) differs
      from that from GENMOD below */
   table habitat / nocum testp=(.101 .340 .104 .455) chisq cl all plots=all;
   exact chisq lrchi;
   weight moosecount;
   table habitat / binomial(level=1);
   table habitat / binomial(level=2);
   table habitat / binomial(level=3);
   table habitat / binomial(level=4);
   ods output OneWayChiSq=chisq1;
   ods output OneWayFreqs=freq1;
   ods output BinomialProp=binprop1;
run;
ods graphics off;
*---part010e;
ods document close;





/* Test if there is selectivity using a log-linear model.
   See Manley et al (2002) on Resource Selection by Animals, Example 4.1 and following */

data moose;
   set moose;
   offset = log(RealProp);   /* adjust for real proportion of each habitat */
run;

proc genmod data=moose;
   title2 'Estimate resource selection functions';
   class habitat;
   /* The type3 test for habitat will be the Likelihood ratio test for habitat selectivity.
      This matches the LR value from JMP. It doesn't match the pearson chi-square test
      statistic from the Proc Freq because of expected small counts  */

   model MooseCount = habitat / offset=offset link=log dist=poisson type3 obstats;
run;




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

ods tagsets.mycolorlatex file='moosehabitat-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=moose(drop=offset);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='moosehabitat-SAS-010-devplot' reset=index;
proc document name=freq1;
   replay \Freq#1\Table1#1\DeviationPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='moosehabitat-SAS-010-freqplot' reset=index;
proc document name=freq1;
   replay \Freq#1\Table1#1\OneWayFreqPlots#1\FreqPlot#1/ dest=listing;
run;
ods graphics off;
ods listing close;

\Freq#1\Table1#1\OneWayFreqPlots#1\FreqPlot#1

ods tagsets.mycolorlatex file='moosehabitat-SAS-010-chitest.tex' (notop nobot);
proc print data=chisq1 noobs label split=" ";
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='moosehabitat-SAS-010-freqs.tex' (notop nobot);
proc print data=freq1(obs=4) noobs label split=" " ;
   var habitat frequency percent testpercent;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='moosehabitat-SAS-010-binprop.tex' (notop nobot);
proc print data=binprop1 noobs label split=" ";
   where name1 in ("_BIN_", "E_BIN", "L_BIN", "U_BIN");
   var label1 cvalue1;
   label label1='Statistic';
   label cvalue1='Value';
run;
ods tagsets.mycolorlatex close;






