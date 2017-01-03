
/* Example of power analysis for simple linear regression on the LOG scale.
   We do this on the log-scale to try and match what happens with Program MONITOR (see my notes) */
/* 2014-07-26 CJS First edition */

/* Suppose we wish to investigate the power of a monitoring design that will
   run for 5 years. At each survey occasion (i.e. every year), we have 1 monitoring
   station, and we make 2 estimates of the population size at the monitoring station in
   each year. The population is expected to start with 1000 animals, and we expect
   that the measurement error (standard error) in each estimate is about 200, i.e. the coefficient
   of variation of each measurement is about 20\% and is constant over time. We
   are interested in detecting increasing or decreasing trends and to start, a 5\%
   decline per year will be of interest. We will assume an UNREALISTIC process error of zero
   so that the sampling error is equal to the total variation in measurements over time.
  
   In this program, we assume that there is NO process error and that all of the variotion 
   sampling error. IN many trend examples, this won't be true SO READ THE CAUTIONS expressed
   in my notes in the TREND chapter. */


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */


dm 'output' clear;
dm 'log'    clear;
proc dataset kill; run;


options orientation=landscape;
ods pdf file='monitor-example1-SAS.pdf' style=styles.printer;

 
title 'Trend Power analysis with no process error';

*---part010b;
/* The SD around the regression line is 200 which corresponds to a cv of 0.20 on the log-scale */
/* The slope of interest (beta1) is 5% decline/year or .05 on the log scale */
%let beta1 = -0.05;       /* 5% decline/year */;
%let beta0 = log(1000);  /* The initial value*/


/* Generate the mean at each X value on the log-scale */
data means;
   do x=0,0,1,1,2,2,3,3,4,4;  /* two measurements each year */
       mu = &beta0 + &beta1*x;
       output;
   end;
run;


proc print data=means;
   title2 'Example 1: Mean response at X values';
run;


proc glmpower data=means;
   title2 'Example 1 Power';
   model mu = x;
   power
     stddev = .20  /* cv on the log-scale */
     alpha = 0.05
     ntotal= 10 /* This needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output1;
run;
*---part010e;


/* We now want a range of slopes. We need to generate means for each slope */
*------------------------ create plot of power for different slopes;
/* We need to generate a set of means for each slope */
*---part020b;
data means;
   do beta1 = -.10 to .10 by 0.02;
      do x=0,0,1,1,2,2,3,3,4,4;
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
     stddev = 0.20
     alpha = 0.05 
     ntotal= 10  /* THis needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output3;
run;
*---part020e;

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


/*******************************************************************************/
/* We now want a range of years. We need to generate means for each slope */
/* This is a real pain in SAS in glmpower because the npower option must be hard
/* coded and cannot refer to a variable */

data means;
   do years = 4 to 10;;
      do x=0 to years, 0 to years;  /* two measurements at each year */
         mu = &beta0 + &beta1*x;
         output;
	  end;
   end;
run;


proc print data=means (obs=20);
   title2 'Power for several different years of study - first few observations';
run;


/* We need to do the counting (in advance) and the use call execute to run the programs
   and append the results together - groan */
proc sort data=means; by years; run;
proc means data=means noprint;
   by years;
   var x;
   output out=npoints n=npoints;
run;
proc print data=npoints;
   title2 'Number of points in each power analysis';
run;
 

%macro runpower(years, npoints);
proc glmpower data=means;
   title2 'Power for several different years';
   where years=&years;
   by years;
   model mu = x;
   power
     stddev = 0.20
     alpha = 0.05 
     ntotal= &npoints  /* THis needs to match the number of data points */
     power = .;
  ods output Output=temp;;
run;

proc append base=glmpower_output4 data=temp force; run;

%mend runpower;

proc datasets;
   delete glmpower_output4;
run;

data _null_;
   set npoints;
   call execute('%runpower(' || put(years,3.0) || "," ||  put(npoints,3.0)||  ");");
run;



proc print data=glmpower_output4;
run;


ods document name=yearplot(write);
proc sgplot data=glmpower_output4;
   title2 'Power for different number of years';
   series x=years y=power;
   refline 0.80 /axis=y;
   xaxis label='Number of years in the study' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Power';
run;
ods document close;

ods pdf close;


/* Now to create the latex output for use with my course notes */
/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;





proc document name=mixed;
   list /levels=all;
run;





ods tagsets.mylatex file='monitor-example1-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output1 label split=" " noobs;
   title2 'Example 1: power values';
   var Alpha StdDev Ntotal Power;
   format power 9.5;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='monitor-example1-SAS-020.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output3 label split=" " noobs;
   title2 'Example 1: power values for different slopes';
   var beta1 Power;
   format power 9.5;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='monitor-example1-SAS-030.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output4 label split=" " noobs;
   title2 'Example 1: power values for different number of years';
   var years Power;
   format power 9.5;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='monitor-example1-SAS-powerplot' reset=index;
proc document name=powerplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='monitor-example1-SAS-yearplot' reset=index;
proc document name=yearplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;






ods listing close;

 



