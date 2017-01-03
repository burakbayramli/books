/* Split-plot design with main plots in a CRD */
/* Effect of pulping method and temperature upon tensile strength of paper */

/* A paper manufacturer is interested in the effect of three
   different pulp preparation methods and four different
   cooking temperatures for the pulp on the tensile
   strength of the resulting paper.

   The equipment that is used for the pulp preparation
   methods only work on large amount of pulp. The equipment
   that is used to cook the pulp can work on smaller batches of material.

   On any one day, the experiment is conducted as follows.
   A batch of pulp is produced by one of the three methods under
   study. The method of pulp preparation is randomized among 9 days
   available for the experiment.

   Within a day, a batch is divided into four sub-batches, and
   cooked at one of the four temperatures. The
   resulting tensile strength of the paper is measured.   */

   2014-03-14 CJS use sgplot and update to sas 9.4

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
footnote ' ';

options date noovp mprint orientation=landscape;
ods pdf file='paper-SAS.pdf' style=styles.printer;

title 'Effect of prep method and cooking temp on tensile strength of paper - Split-plot design';

*---part010b;
data paper;
   infile 'paper.csv' dlm=',' dsd missover firstobs=2;
   input temp method  $ batch $ strength;
   trt = temp || '-' || method;
run;
*---part010e;

proc print data=paper;
   title2 'raw data';
run;

/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=paper;
   title2 'Side-by-side dot plots';
   yaxis label='Tensite Strength'        offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=strength  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=paper; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class temp method;
   var strength;
   table temp*method, strength*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=paper;
   by temp method;
run;

proc means data=paper noprint;  /* find simple summary statistics */
   by temp method;
   var strength;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=temp / group=method;
   yaxis label='Mean Strength' offsetmin=.05 offsetmax=.05;
   xaxis label='Temperature'   offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;

ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=paper plots=all cl ratio;
   title2 'Split-plot analysis with main plots in a crd  - unique batch labels';
   class temp method batch;
   model strength = temp method temp*method / ddfm=kr;
   random batch;
   lsmeans temp   / cl diff adjust=tukey adjdfe=row;
   lsmeans method / cl diff adjust=tukey adjdfe=row;
   lsmeans temp*method / cl diff adjust=tukey adjdfe=row;
   ods output tests3 =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
   footnote 'Because batches are uniquely numbered, a simpler syntax for the model can be used ';
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
*---part100e;
ods document close;

ods graphics on;
proc mixed data=paper plots=all cl ratio;
   title2 'Split-plot analysis with main plots in a crd - traditional method of specifying nested batches';
   class temp method batch;
   model strength = temp method temp*method / ddfm=kr;
   random batch(method);
   lsmeans temp   / cl diff adjust=tukey adjdfe=row;
   lsmeans method / cl diff adjust=tukey adjdfe=row;
   lsmeans temp*method / cl diff adjust=tukey adjdfe=row;
   footnote 'This uses the traditional syntax for main plot experimental units';
   footnote2 'This syntax will work even if main plots are uniquely numbered';
run;
ods graphics off;


ods pdf close;




/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='paper-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=paper(obs=10);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='paper-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='paper-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='paper-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='paper-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='paper-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paper-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=MixedLSMeans noobs label split=" " ;
   var temp method estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='paper-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='paper-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var temp method _temp _method estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='paper-SAS-100-LSMeanjline.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   obfootn  \Print#1\ByGroup1#1\Print#1;
   replay   \Print#1\ByGroup1#1\Print#1;

   obtitle  \Print#1\ByGroup2#1\Print#1;
   obfootn  \Print#1\ByGroup2#1\Print#1;
   replay   \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

/* Estimated variance components */
ods tagsets.mycolorlatex file='paper-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paper-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
