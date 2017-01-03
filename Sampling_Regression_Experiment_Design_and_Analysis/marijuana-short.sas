/* A study was conducted where Student.usages at a college were asked about their 
   personal use of marijuana and if their parents used alcohol and/or marijuana.
   The following data is a collapsed version of the table that appears in the report:
   Marijuana Use in College, Youth and Society, 1979, 323-334
.
   2015-07-05 CJS First Edition

   Lines starting in *--part001b; and *---part001e; are used
   to bracket portions of the R code for inclusion into my
   course notes and are not normally coded. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;

title 'Does parental usage of marijuana assocated with student usage?';
ods pdf file='marijuana-short-SAS.pdf' style=styles.printer; 

*---part001b;
data mj;
   infile 'marijuana-short.csv' dlm=',' dsd missover firstobs=2;
   length parent_usage student_usage $4.;
   input parent_usage student_usage count;
run;
*---part001e;

proc print data=mj;
   title2 'raw data';
run;

*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=mj;
   by parent_usage;
run;
proc means data=mj noprint;
   by parent_usage;
   var count;
   output out=totalcount sum=totalcount;
run;
data mj;
   merge mj totalcount;
   by parent_usage;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=mj;
   title2 'percents computed';
run;
*---part020e;

ods document name=chart1(write);
*---partmosaicb;
/* create the mosaic plot using side-by-side segmented bar chart */
proc sgplot data=mj;
   title2 'Side-by-side segmented barcharts';
   vbar parent_usage / group=student_usage 
                response=percent
                groupdisplay=stack
                stat=sum ;
run;
*---partmosaice;
ods document close;

ods document name=freq1(write);
*---partchisqb;
/* Chi-square test and computing the odds ratio */
proc freq data=mj;
   title2 'Chi-square test';
   table parent_usage*student_usage / nopercent nocol chisq measures cl;
   weight count;
   ods output relativerisks=chirelrisk;
   ods output chisq        =chitest;
run;
*---partchisqe;
ods document close;



 
ods document name=genmod1(write);
*---partgenfitb;
/********* using GENMOD ********/
proc genmod data=mj;
   title2 'fit a generalized linear model';
   class parent_usage student_usage;
   model student_usage = parent_usage / dist=binomial link=logit type3;
   freq count;
   lsmeans parent_usage / diffs cl oddsratio;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
   ods output diffs             =genmodlsmeandiff;
run;
*---partgenfite;
ods document close;



ods pdf close;

/* now create the LaTeX files for inclusion in the course notes */
%include "../../MyLatexTagset.sas"; run;
ods listing close;
title;
footnote;
ods listing;

proc document name=plot2;
 list /levels=all;
run;

ods tagsets.mylatex file='marijuana-short-SAS-data.tex' (notop nobot);
proc print data=mj;
run;
ods tagsets.mylatex close;

/* chisquare test statistics and test results */

ods tagsets.mylatex file='marijuana-short-SAS-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='marijuana-short-SAS-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='marijuana-short-SAS-chitest.tex' (notop nobot);
proc print data=chitest noobs label split=" ";
   where index(statistic,'Like')>0 or statistic='Chi-Square';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='marijuana-short-SAS-chirelrisk.tex' (notop nobot);
proc print data=chirelrisk noobs label split=" ";
run;
ods tagsets.mylatex close;



/* genmod output */
ods tagsets.mylatex file='marijuana-short-SAS-genmodest.tex' (notop nobot);
proc print data=genmodest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='marijuana-short-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='marijuana-short-SAS-genmodlsmeandiff.tex' (notop nobot);
proc print data=genmodlsmeandiff noobs label split=" ";
   var effect parent_usage _parent_usage estimate stderr;
run;

proc print data=genmodlsmeandiff noobs label split=" ";
   var effect parent_usage _parent_usage oddsratio lowerOR upperOR;
run;
ods tagsets.mylatex close;



