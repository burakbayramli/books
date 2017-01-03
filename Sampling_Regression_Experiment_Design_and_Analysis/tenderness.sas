/* Two-factor split-plot design with main-plot factor in blocks */

/* Does  the method of preparation or the presentation affect the perceived tenderness of cooked meat?

This experiment was conducted by first removing loin muscles from 6 different carcasses.
Each muscle was cut into two roasts.
One roast was a quick roast (longer in length and shorter in width)
and one roast was a square roast (same width and length).
Both roasts were cooked to the same temperature on the same day in ovens that are same make and model.
Each roast had 6 slices removed from it
and 6 cubes (1cm x 1cm x 1cm).
One cube and one slice from each roast was served to 6 panelists and they were rated for
initial tenderness (IT).
The same six judges ranked all the roast types and serving method.

While the tenderness scores were obtained from each judge, this example will initially consider
the average score over all 6 judges for each sample.  */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

options nodate noovp orientation=landscape;
ods pdf file='tenderness-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
title 'Tenderness of meat - two factor split-plot with main-plots in blocks';
 
*---part010b;
data tenderness;
   infile 'tenderness.csv' dlm=',' dsd missover firstobs=2;
   length cut $10. serving $10. trt $20.;
   input carcass cut $ serving $ tenderness;
   trt = compress(cut || '.' || serving);
   roast = carcass;  /* generate unique roast numbers */
   if cut='Square' then roast=roast+6;
run;
*---part010e;

proc print data=tenderness;
   title2 'raw data';
run;

  

/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=tenderness;
   title2 'Side-by-side dot plots';
   yaxis label='Tenderness'    offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=tenderness  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=tenderness; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class cut serving;
   var tenderness;
   table cut*serving, tenderness*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=tenderness;
   by cut serving;
run;

proc means data=tenderness noprint;  /* find simple summary statistics */
   by cut serving;
   var tenderness;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=cut / group=serving;
   yaxis label='Mean tenderness'  offsetmin=.05 offsetmax=.05;
   xaxis label='cut'       offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;


/* Fit the models */

ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=tenderness plots=all cl ratio;
   title2 'Analysis using the cut*carcass model term';
   class cut serving carcass;
   model tenderness= cut serving cut*serving / ddfm=kr;
   random carcass cut*carcass;
   lsmeans cut         / diff cl adjust=tukey;
   lsmeans serving     / diff cl adjust=tukey;
   ods output tests3 =Mixed1Test;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed1Lsmeans;
   ods output diffs  =Mixed1Diffs;
   footnote 'Note that the estimated variance components for the blocks is zero - rerun with nobound option';
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed1Diffs,Mixed1Lsmeans,alpha=0.05,sort=yes);
*---part100e;
ods document close;



ods graphics on; 
proc mixed data=tenderness nobound plots=all cl ratio;
   title2 'Analysis using the cut*carcass model term - nobound option';
   class cut serving carcass;
   model tenderness= cut serving cut*serving / ddfm=kr;
   random carcass cut*carcass;
   lsmeans cut         / diff cl adjust=tukey;
   lsmeans serving     / diff cl adjust=tukey;
   ods output diffs=diffs lsmeans=means;
   footnote 'Unbounded option used because the carcass variance component is very close to zero and is estimated as negative';
run;
ods graphics off; run;

/* now to get the joined lined plots in SAS from proc mixed */
%include '../../pdmix800.sas';
%pdmix800(Mixed1Diffs,Mixed1Lsmeans,alpha=0.05,sort=yes);


/* repeat the analysis using the roast as the uniquely labelled experimental unit */
ods graphics on;
proc mixed data=tenderness nobound plots=all cl ratio;
   title2 'Analysis using the roast model term';
   class cut serving carcass;
   model tenderness= cut serving cut*serving / ddfm=kr;
   random carcass roast;
   lsmeans cut         / diff cl adjust=tukey;
   lsmeans serving     / diff cl adjust=tukey;
   lsmeans cut*serving / diff cl adjust=tukey;
run;
ods graphics off; run;
/* now to get the joined lined plots in SAS from proc mixed */
%include '../../pdmix800.sas';
%pdmix800(Mixed1Diffs,Mixed1Lsmeans,alpha=0.05,sort=yes);

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

ods tagsets.mycolorlatex file='tenderness-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=tenderness(obs=10);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='tenderness-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='tenderness-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='tenderness-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='tenderness-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='tenderness-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='tenderness-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=Mixed1LSMeans noobs label split=" " ;
   var cut serving estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='tenderness-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='tenderness-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed1Diffs noobs label split=" " ;
   var cut serving _cut _serving estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='tenderness-SAS-100-LSMeanjline.tex' (notop nobot);
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
ods tagsets.mycolorlatex file='tenderness-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='tenderness-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


