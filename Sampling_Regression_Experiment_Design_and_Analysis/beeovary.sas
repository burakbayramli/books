/* Analysis of Bee-ovary experiment */

/*
This is based on an experiment by S. Hoover, Biological Sciences, Simon Fraser
University.

In normal honey-bee colonies,  the
queen is the main reproductive bee in a colony. Workers cannot mate, but
they can lay unfertilized eggs, which develop into males if reared.
Worker reproduction, while common in queenless colonies, is rare in
queenright colonies, despite the fact that workers are more related
to their own sons than to those of the queen.

In some colonies,  a rare
behavioral syndrome, anarchy occurs, in which substantial worker
production of males occurs in queenright colonies. The level of
worker reproduction in these anarchic colonies is far greater than
in a normal queenright honey-bee colony.

An experiment was conducted where different pheromones were applied to groups of
workers from normal (wild type) and anarchist colonies of bees.

There were 4 anarchist colonies (all that existed at the time at SFU) and 4 wild type
colonies selected at random from all the wild type present at SFU.
A comb was removed from each colony, and incubated overnight.
About 120 bees were taken from those that had emerged from the comb while it was
in the incubator, and 30 were placed in cages, four separate cages from each colony.
One of four types of pheromones were applied to each cage.

After the bees emerged, the bees were scored on
ovary development scores (a scale from 0-4). Between
20-30 bees (a few died) were scored from the 8x4=32 cages.  */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

options nodate noovp orientation=landscape;
ods pdf file='beeovary-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
title 'Effect of pheromone and type upon ovary development of worker bees';
 
*---part010b;
data bees;
   length trt $20;
   infile 'beeovary.csv' dlm=',' dsd missover firstobs=2;
   input ovary_score pheromone $ type $ cage $ colony $;
   trt = compress(type || "-" || pheromone);
run;
*---part010e;

proc print data=bees(obs=40);
   title2 'portion of raw data';
run;



/********************* Analysis of mean ovary score *****************/
*---part015b;
proc sort data=bees;
   by pheromone type colony cage trt;
run;
 
proc means data=bees noprint;
   by pheromone type colony cage trt;
   var ovary_score;
   output out=mean_oscore mean=mean_oscore n=nbees;
run;
*---part015e;
 
proc print data=mean_oscore;
   title2 'mean ovary score';
run;



/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=mean_oscore;
   title2 'Side-by-side dot plots';
   yaxis label='Mean Ovary Score'    offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'           offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=mean_oscore  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=mean_oscore; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class type pheromone;
   var mean_oscore;
   table type*pheromone, mean_oscore*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=mean_oscore;
   by type pheromone;
run;

proc means data=mean_oscore noprint;  /* find simple summary statistics */
   by type pheromone;
   var mean_oscore;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=type / group=pheromone;
   yaxis label='Mean ovary score'  offsetmin=.05 offsetmax=.05;
   xaxis label='Type'              offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;



ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=mean_oscore plots=all cl ratio nobound;
   title2 'Analysis on mean ovary score';
   class type pheromone colony;
   model mean_oscore = type pheromone type*pheromone / ddfm=kr;
   random colony(type);
   lsmeans type*pheromone / cl diff adjust=tukey; 
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

/********************* Analysis of raw ovary score *****************/
ods document name=mixed2(write);
*---part200b;
ods graphics on;
proc mixed data=bees plots=all cl ratio; 
   title2 'analysis on raw ovary score';
   class type pheromone colony;
   model ovary_score = type pheromone type*pheromone / ddfm=kr;
   random colony(type) pheromone*colony(type);
   lsmeans type*pheromone / cl diff adjust=tukey;; 
   ods output tests3 =Mixed2Test;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed2Lsmeans;
   ods output diffs  =Mixed2Diffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed2Diffs,Mixed2Lsmeans,alpha=0.05,sort=yes);
*---part200e;
run;
ods document close;

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

ods tagsets.mycolorlatex file='beeovary-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=bees(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='beeovary-SAS-015.tex' (notop nobot) stylesheet="sas.sty";
proc print data=mean_oscore(obs=10 drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='beeovary-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='beeovary-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='beeovary-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='beeovary-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='beeovary-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='beeovary-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=Mixed1LSMeans noobs label split=" " ;
   var type pheromone estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='beeovary-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='beeovary-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed1Diffs noobs label split=" " ;
   var type pheromone _type _pheromone estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='beeovary-SAS-100-LSMeanjline.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   obfootn  \Print#1\ByGroup1#1\Print#1;
   replay   \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mycolorlatex close;

/* Estimated variance components */
ods tagsets.mycolorlatex file='beeovary-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='beeovary-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/********* Analysis of the raw scores ********/

/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='beeovary-SAS-200-type3.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='beeovary-SAS-200-LSMean.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='beeovary-SAS-200-LSMean-b.tex' (notop nobot);
proc print data=Mixed2LSMeans noobs label split=" " ;
   var type pheromone estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='beeovary-SAS-200-LSMeandiff.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='beeovary-SAS-200-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   var type pheromone _type _pheromone estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='beeovary-SAS-200-LSMeanjline.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   obfootn  \Print#1\ByGroup1#1\Print#1;
   replay   \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mycolorlatex close;

/* Estimated variance components */
ods tagsets.mycolorlatex file='beeovary-SAS-200-varcomp.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='beeovary-SAS-200-diagnostics' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


