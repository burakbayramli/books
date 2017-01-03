/* Example of a split-plot in time */

/* A former gas station was being torn down and the owners 
   wished to build an apartment complex on the property. 
   Unfortunately, the soil was contaminated and before a new 
   complex can be built, the soil must be 'cleaned up'. Over 
   time, the organic solvents will naturally degrade, but 
   certain fungi can speed up the process. 

   In this experiment, 12 sites on the property were 
   randomly selected. Three sites were randomly 
   assigned to the four fungal treatments (Control, and three 
   different combinations of fungi in different 
   concentrations). The amount of organic solvent was measured 
   at 1, 2, 3 and 4 months post treatment. */



/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

options nodate noovp orientation=landscape;
ods pdf file='fungus-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 

title 'Split-plot in time - soil remediation';

*---part010b;
data fungus;
   infile 'fungus.csv' dlm=',' dsd missover firstobs=2;
   length fungus trt $20.;
   input fungus $ time site  contam;
   trt = compress(fungus || '-' || put(time,1.0));
   keep fungus trt site time contam ;
run;
*---part010e;

proc print data=fungus;
   title2 'raw data';
run;


/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=fungus;
   title2 'Side-by-side dot plots';
   yaxis label='Contaminant Load'    offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'           offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=contam  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=fungus; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class fungus time;
   var contam;
   table fungus*time, contam*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=fungus;
   by fungus time;
run;

proc means data=fungus noprint;  /* find simple summary statistics */
   by fungus time;
   var contam;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=time / group=fungus;
   yaxis label='Mean Contaminant Load'  offsetmin=.05 offsetmax=.05;
   xaxis label='Time'                   offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;


/* Now for the analysis */
ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=fungus plots=all cl ratio;
   title2 'Analysis as a classical split-plot in time';
   class fungus time site;
   model contam = fungus time time*fungus / ddfm=kr;
   random site(fungus) ;
   lsmeans fungus  / pdiff cl adjust=tukey;
   lsmeans time    / pdiff cl adjust=tukey;
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



ods graphics on;
proc mixed data=fungus plots=all cl ratio;
   title2 'Analysis as a repeated measures with a compound symmetric structure';
   title3 'This will give the EXACT same results as the split-plot in time above';
   class fungus time site;
   model contam = fungus time fungus*time / ddfm=kr;
   repeated  time / type=cs r rcorr subject=site;
run;
ods graphics off;


ods graphics on;
proc mixed data=fungus plots=all cl ratio;
   title2 'Analysis as a repeated measures with an ar(1) correlation structure';
   /* Because this is a split-plot in time, the assumption of compound symmetry
      may not be suitable. Consequently, try to fit an ar(1) structure to the
      the repeated measurements over time */
   class fungus time site;
   model contam = fungus time time*fungus / ddfm=kr;
   repeated  time / type=ar(1) r rcorr subject=site;
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

ods tagsets.mycolorlatex file='fungus-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fungus(obs=10);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='fungus-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='fungus-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='fungus-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='fungus-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='fungus-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fungus-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=Mixed1LSMeans noobs label split=" " ;
   var fungus time estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='fungus-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='fungus-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed1Diffs noobs label split=" " ;
   var fungus time _fungus _time estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='fungus-SAS-100-LSMeanjline.tex' (notop nobot);
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
ods tagsets.mycolorlatex file='fungus-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fungus-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

