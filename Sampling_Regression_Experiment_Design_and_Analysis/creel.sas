/* Simple Random Sample - Creel survey */
/* 2015-07-04 Updated to more recent sas code */

title 'Creel Survey - Simple Random Sample';

/*
For management purposes, it is important to estimate the 
total catch by recreational fishers.
Unfortunately, there is no central registry of fishers, nor 
is there a central reporting station.
Consequently, surveys are often used to estimate the total 
catch. 

An access survey was conducted to estimate the total catch at 
a lake in British Columbia. Fortunately, access to the lake 
takes place at a single landing site and most anglers use 
boats in the fishery. An observer was stationed at the 
landing site, but because of time constraints, could only 
interview a portion of the parties returning, but was able to 
get a total count of the number of parties fishing on that 
day. A total of 168 boats (fishing parties)
arrived at the landing during the 
day, of which 30 were sampled. The decision to sample an 
party was made using a random number table as the boats
returned.

The objectives are to estimate the total number of anglers 
and their catch and to estimate the proportion of boat trips 
that had sufficient life-jackets for the members on the trip.
Here is the raw data. */

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */

ods pdf file='creel.pdf' style=styles.printer;

*---part001b; 
data  creel;   /* read in the survey data */
   infile 'creel.csv' dlm=',' dsd missover;
   input angler catch lifej $;
   enough = 0;
   if lifej = 'yes' then enough = 1;
*---part001e;
 
proc print data=creel;
   title2 'raw data';
run;



/* add the sampling weights to the data set. The sampling
   weights are defined as N/n for an SRSWOR */
*---part002b; 
data creel;
   set creel;
   sampweight = 168/30;
run;
*---part002e; 

*---part003b;
proc surveymeans data=creel
      total=168   /* total population size */
      mean clm    /* find estimates of mean, its se, and a 95% confidence interval */
      sum  clsum  /* find estimates of total,its se, and a 95% confidence interval */
     ;
   var angler catch lifej ; /* estimate mean and total for numeric variables, proportions for char variables */
   weight sampweight;
   /* Note that it is not necessary to use the coded 0/1 variables in this procedure */
   ods output statistics=creelresults;
run;
*---part003e;

ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=ttest1;
 list /levels=all;
run;

ods tagsets.mylatex file="creel-SAS-003.tex" (notop nobot) /* newfile=table */;
proc print data=creelresults noobs label split=" ";
   var varname varlevel mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean Sum LowerCLSum stderr upperCLmean stddev UpperCLsum 10.3;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;

