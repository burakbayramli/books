/* Compute power curves for a single factor RCB ANOVA */

/* There are 3 different herbicides that were tested against control
   and no evidence of a differece in the mean yield was found among
   the three herbicides.

   Design future experiments to distinguish among the means of the 
   three herbicides

   The standard deviation (extracted from the sqrt(MSE) from the original analysis 
   is about 0.174. A biologically important difference is 0.2 */

options orientation=landscape;
ods pdf file='herbicide-power-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Herbicide power analysis';
options nodate  noovp;

%let alpha=.05;
%let power=.80;



*---part001b;
proc power;
   title2 'Using a base difference of 0.2 mm';
   onewayanova
      groupmeans = 0 | .1 | .2  /* list the group means that differ by biological effect*/
      stddev = .174    /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onewayanova statement - don't forget it */
   ods output Output=power1;
   footnote 'This configuration has the worst power and so the largest possible sample size';
   footnote2 'This power computation is APPROXIMATE as not adjustment has been made';
   footnote3 'for the loss in df due to blocking';
run;
*---part001e;


proc power;
   title2 'Using a base difference of 0.2 mm with a different configuration of means';
   onewayanova
      groupmeans = 0 | .2 | .2  /* list the group means */
      stddev = .174       /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onewayanova statement - don't forget it */
   footnote 'This configuration has the best power and so the smallest  possible sample size';
run;

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

ods tagsets.mycolorlatex file='herbicide-power-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power1 noobs split=' ' label ;
  var mean1 mean2 mean3 stddev nominalpower power Ntotal;
run;
ods tagsets.mycolorlatex close;
