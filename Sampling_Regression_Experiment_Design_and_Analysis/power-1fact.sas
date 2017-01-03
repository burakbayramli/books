/* Compute power curves for a single factor CRD ANOVA */

/* There are 6 host species in whose nests cuckoo birds lay eggs.
   What sample size is needed to detect a difference between 21 and 23 mm.
   The standard deviation is about 1 mm. */

*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


options nodate noovp orientation=landscape;
ods pdf file='power-1fact-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Cuckoo egg size power analysis';

*---part010b;
proc power;
   title2 'Using a base difference of 2 mm';
   onewayanova
      groupmeans = 21 | 22 | 22 | 22 | 22 | 23  /* list the group means */
      stddev = 1       /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onewayanova statement - don't forget it */
   ods output Output=Power10;
   footnote 'This configuration has the worst power and so the largest possible sample size';
run;
*---part010e;

*---part020b;
proc power;
   title2 'Using a base difference of 2 mm with a different configuration of means';
   onewayanova
      groupmeans = 21 | 21 | 21 | 23 | 23 | 23  /* list the group means */
      stddev = 1       /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onewayanova statement - don't forget it */
   ods output Output=Power20;
   footnote 'This configuration has the best power and so the smallest  possible sample size';
run;
*---part020e;

ods pdf close;



/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=power10;
 list /levels=all;
run;


ods tagsets.mycolorlatex file='power-1fact-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power10 noobs split=' ' label ;
  var alpha mean1-mean6 stddev nominalpower power  Ntotal;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='power-1fact-SAS-020.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power20 noobs split=' ' label ;
  var alpha mean1-mean6 stddev nominalpower power  Ntotal;
run;
ods tagsets.mycolorlatex close;
