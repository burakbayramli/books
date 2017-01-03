/* Power analysis for the stream slope example */

/*
  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusing by LaTex and usually are not coded.
*/

dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


title 'Stream slope -   power analysis';
options nodate  noovp orientation=landscape;
ods pdf file='power-stream-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


/* Method 1 - use the difference data directly */
/* From the previous analyses, the 
   standard deviation of the DIFFERENCES is around 5.701
   the biologically significant difference in means is 2
   the required power is .80
   We wish to solve for n, the sample size, the number of streams */


*---part001b;
proc power;
   title2 'Finding power based on difference of 2';
   onesamplemeans
      mean = 2         /* list the group means that differ by biological effect*/
      stddev = 5.701    /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onesamplemeans statement - don't forget it */
   ods output Output=power1;
 run;
*---part001e;

*---part011b;
proc power;
   title2 'Finding power based on difference of 2';
   pairedmeans
      pairedmeans = (6, 8) /* list two group means that differ by biological effect*/
      pairedstddevs = (14.9, 12.) /* what is the standard deviation */
      corr = 0.92       /* what is correlation between measurements? */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      npairs = .       /* solve for power */
   ;    /* end of the onesamplemeans statement - don't forget it */
   ods output Output=power2;
 run;
*---part011e;


ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=power1;
 list /levels=all;
run;


ods tagsets.mycolorlatex file='power-stream-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power1 noobs split=' ' label ;
  var alpha mean stddev sides nullmean nominalpower power Ntotal;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='power-stream-SAS-011.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power2 noobs split=' ' label ;
  var analysis alpha mean1 mean2 stddev1 stddev2 corr sides /*nulldiff */ nominalpower power Npairs;
run;
ods tagsets.mycolorlatex close;
