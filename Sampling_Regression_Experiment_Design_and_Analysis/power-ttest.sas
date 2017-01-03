/* Illustration of a power analysis for a two-sample ttest using SAS */

/* We wish to find the sample size to detect a difference of 50 lbs in the mean weight of cattle
   between a hormone and control group. The standard deviation is 100.
   The test is to be performed at alpha=.05 with a target power of 80%. */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

options  orientation=landscape nodate noovp;
ods pdf file='power-ttest-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part010b;
proc power;
  title 'Power analysis for hormone example';
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=50   /* size of difference to be detected */  
     stddev=100    /* the standard deviation within each group */
     power=.80     /* target power of 80% */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     ntotal=.      /* solve for the total sample size assuming equal sample sizes in both groups */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  ods output Output=Power10;
run;
*---part010e;

ods document name=power20(write);
*---part020b;
ods graphics on;
proc power;
  title 'Power analysis for hormone example with various sized differences';
  /* We vary the size of the difference to see what sample size is needed */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=30 to 150 by 10   /* size of difference to be detected */  
     stddev=100    /* the standard deviation within each group */
     power=.80     /* target power of 80% */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     ntotal=.      /* solve for the total sample size assuming equal sample sizes in both groups */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  plot x=effect xopts=(ref=50 crossref=yes);  /* plot the sample size vs effect size and draw ref lines at effect=50 */
  ods output output=power20;
run;
ods graphics off;
*---part020e;
ods document close;

ods document name=power30(write);
*---part030b;
ods graphics on;
proc power;
  title 'Power analysis for hormone example with various sample sizes';
  /* We vary the total sample size to see what power is obttained */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=50   /* size of difference to be detected */  
     stddev=100    /* the standard deviation within each group */
     power=.       /* solve for power */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     ntotal=50 to 200 by 10     /* total sample size assuming equal sample sizes in both groups */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  plot x=n  yopts=(ref=.80 crossref=yes);  /* plot the power as a function of sample size and draw ref line at 80% power */
  ods output output=power30;
run;
ods graphics off;
*---part030e;
ods document close;

ods pdf close;



/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=power30;
 list /levels=all;
run;


ods tagsets.mycolorlatex file='power-ttest-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power10 noobs split=' ' label ;
  var alpha meandiff stddev sides nulldiff nominalpower power Ntotal;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='power-ttest-SAS-020.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power20 /* noobs split=' ' label */;
  var alpha meandiff stddev sides nulldiff nominalpower power Ntotal;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='power-ttest-SAS-020b' reset=index;
proc document name=power20;
   replay \Power#1\TwoSampleMeans#1\PlotStatement1#1\NTotalMeanDiff#1\PowerPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




ods tagsets.mycolorlatex file='power-ttest-SAS-030.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power30 /* noobs split=' ' label */;
  var alpha meandiff stddev sides Ntotal nulldiff  power;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='power-ttest-SAS-030b' reset=index;
proc document name=power30;
   replay \Power#1\TwoSampleMeans#1\PlotStatement1#1\PowerNTotal#1\PowerPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
