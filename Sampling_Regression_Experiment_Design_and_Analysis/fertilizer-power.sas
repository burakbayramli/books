/* Simple Linear Regression - power analysis */
/* 2014-07-18 CJS First Edition */

/* Let us return to the example of the yield of tomatoes vs. the amount of fertilizer. 
   We wish to design an experiment to detect a slope of 1 (the effect size). 
   From past data, the standard deviation of
   values about the regression line is about 2 units (the standard deviation of the residuals). 

   We have enough money to plant 12 plots with levels of fertilizer ranging from 10 to 20. 
   How does the power compare under different configuration
   of choices of fertilizer levels. More specifically, how does the power compare between using
   fertilizer levels 
      (10, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 20), i.e. an even distribution of
          levels of fertilizer, and 
      (10, 10, 12, 12, 14, 14, 16, 16, 18, 18, 20, 20), i.e. doing two replicates
         at each level of fertilizer but doing fewer distinct levels? */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */



dm 'output' clear;
dm 'log'    clear;
proc dataset kill; run;

options orientation=landscape;
ods pdf file='fertilizer-power-SAS.pdf' style=styles.printer;
 
title 'Simple Linear Regression Power analysis for Fertilizer problem';

/* The SD around the regression line is 4 */
/* The slope of interest (beta1) is 1 */

%let beta1 = 1;
%let beta0 = 10;  /* The intercept is arbitrary */

/* Generate the mean at each X value */
data means;
   do x=10, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 20;
       mu = &beta0 + &beta1*x;
       output;
   end;
run;
 
proc print data=means;
   title2 'Example 1: Mean response at x values';
run;

proc glmpower data=means;
   title2 'Example 1 Power';
   model mu = x;
   power
     stddev = 4
     alpha = 0.05
     ntotal= 12 /* This needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output1;
run;




/*****************************************************************************************************/
/* Example 2 - A different configuration */

/* The SD around the regression line is 4 */
/* The slope of interest (beta1) is 1 */

*---part010b;
%let beta1 = 1;
%let beta0 = 10;

/* Generate the mean at each X value */
data means;
   do x=10, 10, 12, 12, 14, 14, 16, 16, 18, 18, 20, 20;
      mu = &beta0 + &beta1*x;
      output;
   end;
run;
*---part010e;
 
proc print data=means;
   title2 'Example 2: mean response at x values';
run;

*---part015b;
proc glmpower data=means;
   title2 'Example 2 Power';
   model mu = x;
   power
     stddev = 4
     alpha = 0.05 
     ntotal= 12  /* THis needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output2;
run;
*---part015e;


*------------------------ create plot of power for different slopes;
/* We need to generate a set of means for each slope */
data means;
   do beta1 = 0 to 2 by 0.25;
      do x=10, 10, 12, 12, 14, 14, 16, 16, 18, 18, 20, 20;
         mu = &beta0 + beta1*x;
         output;
	  end;
   end;
run;
 
proc print data=means (obs=20);
   title2 'Power for several slopes - first few observations';
run;

proc glmpower data=means;
   title2 'Power for several slopes';
   by beta1;
   model mu = x;
   power
     stddev = 4
     alpha = 0.05 
     ntotal= 12  /* THis needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output3;
run;

proc print data=glmpower_output3;
run;

ods document name=powerplot(write);
proc sgplot data=glmpower_output3;
   title2 'Power for several slopes';
   series x=beta1 y=power;
   refline 0.80 /axis=y;
   xaxis label='Slope' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Power';
run;
ods document close;

ods pdf close;

/* Now to create the latex output for use with my course notes */
/* Create LaTeX files for inclusion by my notes */
%include "../../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;


proc document name=mixed;
   list /levels=all;
run;



ods tagsets.mylatex file='fertilizer-power-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output1 label split=" " noobs;
   title2 'Example 1: power values';
   var Alpha StdDev Ntotal Power;
   format power 9.5;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fertilizer-power-SAS-020.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output2 label split=" " noobs;
   title2 'Example 2: power values';
   var Alpha StdDev Ntotal Power;
   format power 9.5;
run;
ods tagsets.mylatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fertilizer-power-SAS-powerplot' reset=index;
proc document name=powerplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
 

