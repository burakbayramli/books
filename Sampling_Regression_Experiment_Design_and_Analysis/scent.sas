/* Split plot, repeated treatments at sub-plot level */

/* Can pleasant aromas help a student learn better? Hirsch and Johnston, of the
Smell & Taste Treatment and Research Foundation, believe that the presence of
a floral scent can improve a person's learning ability in certain situations. They
also believe that this may differ between the two sexes. i

In their experiment, 21 people worked through a set of pencil and paper
mazes six times, three times while wearing a floral-scented mask and three
times wearing an unscented mask. The order in which the mazes were done was
randomized as was the wearing or not wearing a mask.

Participants put on their masks one minute before starting the first trial
in each group to minimize any distracting effects. Subjects recorded whether
they found the scent inherently positive, inherently negative, or if they were
indifferent.

Testers measured the length of time it took subjects to complete
each of the six trials. */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;


options nodate noovp orientation=landscape;
ods pdf file='scent-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Effect of scent upon completion times of a maze';

*---part010b;
data scent;
   length sex $1.  mask $10. trt $20;
   infile 'scent.csv' dlm=',' dsd missover firstobs=2;
   input subject sex $ smoke $ opinion $ age $ u1 u2 u3 s1 s2 s3; /* u=unscented, s=scented*/
   trt = sex || "-" || mask;
   mask = 'unscented'; 
   trt = compress(sex || "-" || mask);
   time=u1; trial=1; output;
   time=u2; trial=2; output;
   time=u3; trial=3; output;
   mask = 'scented';
   trt = compress(sex || "-" || mask);
   time = s1; trial=1; output;
   time = s2; trial=2; output;
   time = s3; trial=3; output;
   drop u1 u2 u3 s1 s2 s3;
run;
*---part010e;

proc print data=scent;
   title2 'raw data read in';
run;

/***************************************************************************/
/* Method 1 - analysis on averages */
*---part015b;
proc sort data=scent;
   by subject trt;
run;
  
proc means data=scent noprint;
   by subject trt;
   var time;
   output out=mean_scent mean=mean_time;
   id sex mask smoke  opinion  age;
run;
*---part015e;


/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=mean_scent;
   title2 'Side-by-side dot plots';
   yaxis label='Mean time to complete maze'    offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'                     offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=mean_time  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=mean_scent; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class sex mask;
   var mean_time;
   table sex*mask, mean_time*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=mean_scent;
   by sex mask;
run;

proc means data=mean_scent noprint;  /* find simple summary statistics */
   by sex mask;
   var mean_time;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=sex / group=mask;
   yaxis label='Mean mean_scent'  offsetmin=.05 offsetmax=.05;
   xaxis label='Sex'       offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;



ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=mean_scent plots=all cl ratio;
   title2 'Split plot analysis on mean of repeated trials';
   class sex subject mask;
   model mean_time = sex mask sex*mask / ddfm=kr;
   random subject(sex);
   lsmeans sex / diff adjust=tukey cl;
   lsmeans mask / diff adjust = tukey cl;
   lsmeans sex*mask / diff adjust=tukey cl;
   ods output tests3 =Mixed1Test;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed1Lsmeans;
   ods output diffs  =Mixed1Diffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed1Diffs,Mixed1Lsmeans,alpha=0.05,sort=yes);
*---part100e;
ods document close;
run;



/***************************************************************************/
/* Method 2 - analysis on individual values */
ods graphics on;
proc mixed data=scent plot=all cl ratio;
   title2 'Split-plot analysis on individual values';
   class sex subject mask trial;
   model time = sex mask sex*mask / ddfm=kr;
   random subject(sex) mask*subject(sex);
   lsmeans sex / diff adjust=tukey cl;
   lsmeans mask / diff adjust = tukey cl;
   lsmeans sex*mask / diff adjust=tukey cl;
run;
ods graphics off;

ods pdf close;




/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed2;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='scent-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=scent(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='scent-SAS-015.tex' (notop nobot) stylesheet="sas.sty";
proc print data=mean_scent(obs=10 drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='scent-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='scent-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='scent-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='scent-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='scent-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='scent-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=Mixed1LSMeans noobs label split=" " ;
   var sex mask estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='scent-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='scent-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed1Diffs noobs label split=" " ;
   var sex mask _sex _mask estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='scent-SAS-100-LSMeanjline.tex' (notop nobot);
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
ods tagsets.mycolorlatex file='scent-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='scent-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


