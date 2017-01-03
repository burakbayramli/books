/* Single mean under pseudo-replication or sub-sampling */
dm 'output' clear;
dm 'log'    clear;

/* This example is based on work conduced in the laboratory of 
   Prof. Lynn Quarmby at Simon Fraser University 
   http://www.sfu.ca/mbb/People/Quarmby/. 
   Her research focus is on the the mechanism by which 
   cells shed their cilia (aka flagella) in response to stress working
   with the unicellular alga Chlamydomonas. 

   Microphotographs of the algae are taken, and the 
   length of one or two flagellum of each cell is measured. */

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */



title h=1.5 'Estimate mean flagella length';
options nodate orientation=landscape;
ods document name=work.output(write);  /* enable selection of output from SAS */
ods pdf file='flagella1.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data flagella;
   infile 'flagella1.csv' dlm=',' dsd missover firstobs=2;
   length cell $20.;
   input cell $ length1 length2;
run;

proc print data=flagella(obs=20);
   title2 'Part of the raw data';
run;
*---part001e;

/***************** Method 1 ********************/
/* Analyze the averages */
*---part010b;
data flagella;
   set flagella;
   avg_length = mean(length1,length2);
run;

proc print data=flagella (obs=10);
   title2 'Part of the raw data after avg taken';
run;
*---part010e;

*---part011b;
proc univariate data=flagella cibasic;
   title2 'Estimate the mean using average of averages';
   var avg_length;
   histogram avg_length;
run;
*---part011e;



/******************** Method 2 *****************/
/* Use the individual measurements */
 
*---part020b;
data stack_flagella;
   set flagella;
   length = length1; output;
   length = length2; output;
   drop length1 length2 avg_length;
run;

proc print data=stack_flagella(obs=20);
   title2 'Stacked data';
run;
*---part020e;
 
*---part021b;
proc mixed data=stack_flagella;
   title2 'Analyze the raw measurements';
   class cell;
   model length = ;  /* intercept is implicit */
   random cell;
   estimate 'Overall mean' intercept 1 / cl;
run;
*---part021e;

ods pdf close;
ods document close;





/* Now to replay selected portions of the output. This would not normally
   be done by the user, but I used this for the latex documents */
/* First see the names of the objects in the document store */

proc document name=work.output;
  list / levels=all;
run;
quit;



options nodate nonumber;

ods pdf file='flagella1-SAS-001.pdf';
proc document name=work.output;
   replay  \Print#1\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella1-SAS-010.pdf';
proc document name=work.output;
   replay  \Print#2\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella1-SAS-011.pdf' startpage=never;
proc document name=work.output;
   obpage  \Univariate#1\avg_length#1\Moments#1 / after delete ;
   replay  \Univariate#1\avg_length#1\Moments#1 / levels=all dest=(pdf);
   obpage   \Univariate#1\avg_length#1\BasicIntervals#1 / delete;
   replay   \Univariate#1\avg_length#1\BasicIntervals#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella1-SAS-020.pdf';
proc document name=work.output;
   replay  \Print#3\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella1-SAS-021.pdf' startpage=never;
proc document name=work.output;
   obpage   \Mixed#1\CovParms#1 / after delete ;
   replay   \Mixed#1\CovParms#1 / levels=all dest=(pdf);
   obpage    \Mixed#1\Estimates#1 / delete;
   replay    \Mixed#1\Estimates#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;
