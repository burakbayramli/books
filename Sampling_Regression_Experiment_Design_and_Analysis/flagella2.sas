/* Single-Factgor ANOVA under pseudo-replication or sub-sampling */
dm 'output' clear;
dm 'log'    clear;

/* This example is based on work conduced in the laboratory of 
   Prof. Lynn Quarmby at Simon Fraser University 
   http://www.sfu.ca/mbb/People/Quarmby/. 
   Her research focus is on the the mechanism by which 
   cells shed their cilia (aka flagella) in response to stress working
   with the unicellular alga Chlamydomonas. 

   Microphotographs of the algae are taken, and the 
   length of one or two flagellum of each cell is measured. 

   Multiple cells of each variant are measured */

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */



title h=1.5 'Compare mean flagella length among variants';
options nodate orientation=landscape;
ods document name=work.output(write);  /* enable selection of output from SAS */
ods pdf file='flagella2.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data flagella;
   infile 'flagella2.csv' dlm=',' dsd missover firstobs=2;
   length variant cell $20.;
   input variant $ cell $ length1 length2;
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

/* Get side-by-side dot plots */
*---part011b;
proc gplot data=flagella;
   title2 'side-by-side dot plots - all of data';
   axis1 label=(a=90 r=0 'Average length');
   axis2 label=(         'Variant') offset=(1 cm, 1 cm);
   plot avg_length * variant / vaxis=axis1 haxis=axis2;
run;
*---part011e;

*---part012b;
/* Remove the outlier */
data flagella;
   set flagella;
   if variant = 'A' and avg_length < 10 then delete;
run;
*---part012e;

/* Get side-by-side dot plots */
*---part013b;
proc gplot data=flagella;
   title2 'side-by-side dot plots - after outlier removed';
   axis1 label=(a=90 r=0 'Average length');
   axis2 label=(         'Variant') offset=(1 cm, 1 cm);
   plot avg_length * variant / vaxis=axis1 haxis=axis2;
run;
*---part013e;


/* Compute summary statistics */
*---part014b;
proc tabulate data=flagella;
   title2 'summary statistics after removing outliers';
   class variant;
   var avg_length;
   table variant, avg_length*(n*f=4.0
                    mean*f=6.2 std*f=6.2);
run;
*---part014e;


*---part018b;
ods graphics on;
proc mixed data=flagella;
   title2 'Analyze the average length';
   class variant cell;
   model avg_length = variant / residual;
   lsmeans variant / cl diff adjust=tukey;
run;
ods graphics off;
*---part018e;

/******************** Method 2 *****************/
/* Use the individual measurements */
 
*---part030b;
data stack_flagella;
   set flagella;
   length = length1; output;
   length = length2; output;
   drop length1 length2 avg_length;
run;

proc print data=stack_flagella(obs=20);
   title2 'Stacked data';
run;
*---part030e;
 
*---part031b;
ods graphics on;
proc mixed data=stack_flagella;
   title2 'Analyze the raw measurements';
   class variant cell;
   model length = variant / residual;  
   random cell(variant);
   lsmeans variant / cl diff adjust=tukey;
run;
ods graphics off;
*---part031e;

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

ods pdf file='flagella2-SAS-001.pdf';
proc document name=work.output;
   replay  \Print#1\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella2-SAS-010.pdf';
proc document name=work.output;
   replay  \Print#2\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella2-SAS-011.pdf';
proc document name=work.output;
   replay  \Gplot#1\GPLOT#1/ levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-013.pdf';
proc document name=work.output;
   replay  \Gplot#2\GPLOT1#1/ levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-014.pdf';
proc document name=work.output;
   replay  \Tabulate#1\Report#1\Table#1/ levels=all dest=(pdf);
run;
quit;
ods pdf close;


ods pdf file='flagella2-SAS-018.pdf' startpage=never;
proc document name=work.output;
   obpage   \Mixed#1\Tests3#1  / after delete ;
   replay   \Mixed#1\Tests3#1  / levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-019.pdf' startpage=never;
proc document name=work.output;
   obpage   \Mixed#1\LSMeans#1 / delete;
   replay   \Mixed#1\LSMeans#1 / levels=all dest=(pdf);
   obpage   \Mixed#1\Diffs#1   / after delete;
   replay   \Mixed#1\Diffs#1   / levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-020.pdf';
proc document name=work.output;
   replay  \Mixed#1\ResidualPlots#1\ResidualPanel#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;



ods pdf file='flagella2-SAS-030.pdf';
proc document name=work.output;
   replay  \Print#3\Print#1 / levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-031.pdf' startpage=never;
proc document name=work.output;
   obpage   \Mixed#2\CovParms#1 / after delete ;
   replay   \Mixed#2\CovParms#1 / levels=all dest=(pdf);
   obpage    \Mixed#2\Tests3#1  / delete;
   replay    \Mixed#2\Tests3#1  / levels=all dest=(pdf);
run;
quit;
ods pdf close;

ods pdf file='flagella2-SAS-032.pdf' startpage=never;
proc document name=work.output;
   obpage   \Mixed#2\LSMeans#1 / after delete ;
   replay   \Mixed#2\LSMeans#1 / levels=all dest=(pdf);
   obpage   \Mixed#2\Diffs#1    / delete;
   replay   \Mixed#2\Diffs#1    / levels=all dest=(pdf);
run;
quit;
ods pdf close;
