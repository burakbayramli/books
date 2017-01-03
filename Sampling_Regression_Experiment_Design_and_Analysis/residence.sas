/* Stream Residence Example:  Two factor, completely randomized design */

/* This example is taken from:
   Trouton, Nicole (2004).
   An investigation into the factors influencing escapement estimation for chinook salmon
   (Oncorhynchus tshawytscha) on the Lower Shuswap River, British Columbia.
   M.Sc. Thesis, Simon Fraser University.

   Spawning residence time is an important value in managing
   Pacific salmon. A standard procedure to estimate the
   total number of fish that have returned to spawn (the
   escapement) is to estimae the total number of fish-days
   spent by salmon in a stream by aerial flights and then
   divide this number by the spawning residence time.
   This procedure is called
   the Area-under-the-curve (AUC) method of estimating escapement.

   Trouton captured and inserted radio transmitters into a sample of
   chinook salmon on the Lower Shuswap River in British Columbia,
   Canada. Then she rowed along the stream on a daily basis
   with a radio receiver to see how long each fish spent on
   the spawning grounds.

   Approximately 60, 70, and 150 fish were radio tagged in 2000, 2001, and
   2002 respectively, approximately equally split between males
   and female in each year. Not all fish survived the radio insertion
   and not all fish spawned in the reaches of the river surveyed by
   the radio receiver so the number of data points actually
   measured each year is less than this number.  */

dm 'output' clear;
dm 'log'    clear
proc datasets kill;

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='residence-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;



title 'Spawning residence time - two factor unblanced CRD example';

*---part001b; 
data residence;
   infile 'residence.csv' dlm=',' dsd missover firstobs=2;
   length trt $8.;
   input time sex $ year $;
   substr(trt,1,1) = sex;
   substr(trt,2,1) = '-';
   substr(trt,3,4) = year;
run;
*---part001e;


proc print data=residence;
   title2 'raw data';
run;



ods document name=dotplot(write);
*---part010b; 
proc sgplot data=residence;
   title2 'Side-by-side dot plots';
   yaxis label='Residence Time' offsetmin=.05 offsetmax=.05;
   xaxis label='Sex and Year'           offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=time  /  markerattrs=(symbol=circlefilled);
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Find some simple summary statistics.*/ 
ods document name=meanstable(write);
*---part020b;
proc tabulate data=residence; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class sex year;
   var time;
   table sex*year, time*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;
*---part020e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a crd, it is easy to create the se and 95% for each mean */
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part030b;
proc sort data=residence;
   by sex year;
run;

proc means data=residence noprint;  /* find simple summary statistics */
   by sex year;
   var time;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=year / group=sex;
   highlow x=year high=uclm low=lclm  / group=sex;
   yaxis label='Residence time' offsetmin=.05 offsetmax=.05;
   xaxis label='Year'           offsetmin=.05 offsetmax=.05;
run;
*---part030e;
ods document close;



ods document name=glm1(write); 
*---part100b;
ods graphics on; 
proc glm data=residence plots=all;
   title2 'analysis';
   class sex year;
   model time = sex year sex*year;
   lsmeans sex*year / cl pdiff adjust=tukey lines;
   lsmeans sex      / cl pdiff adjust=tukey lines;
   lsmeans year     / cl pdiff adjust=tukey lines;
run; 
ods graphics off;
*---part100e;
ods document close;



/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=residence plots=all;
   title2 'Mixed model balanced';
   class sex year;  /* class statement identifies factors */
   model time = sex year sex*year / ddfm=kr;
   lsmeans sex*year /  cl adjust=tukey ;
   lsmeans sex      /  cl adjust=tukey ;
   lsmeans year     /  cl adjust=tukey ;
   ods output tests3 =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
run; 
ods graphics off;

/* Get a joined lines plot */
%include '../pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
*---part200e;
ods document close;






ods pdf close;




/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=glm1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='residence-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=residence(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='residence-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='residence-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='residence-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Output from GLM */
ods tagsets.mycolorlatex file='residence-SAS-100-overall.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\time#1\OverallANOVA#1; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\time#1\OverallANOVA#1;
   replay \GLM#1\ANOVA#1\time#1\OverallANOVA#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-type3.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\time#1\ModelANOVA#2; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\time#1\ModelANOVA#2;
   replay \GLM#1\ANOVA#1\time#1\ModelANOVA#2;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMint.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeans#1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMintci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-100-LSMintdiff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_year#1\time#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_year#1\time#1\Diff#1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMintdiffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='residence-SAS-100-LSMintdiffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMintjline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMLines#1;
   replay \GLM#1\LSMEANS#1\sex_year#1\time#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-100-LSMme1.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\time#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\time#1\LSMeans#1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme1ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-100-LSMme1diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\time#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\time#1\Diff#1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme1diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='residence-SAS-100-LSMme1diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme1jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\time#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\time#1\LSMLines#1;
   replay \GLM#1\LSMEANS#2\sex#1\time#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;








ods tagsets.mycolorlatex file='residence-SAS-100-LSMme2.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\year#1\time#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\year#1\time#1\LSMeans#1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme2ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\year#1\time#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\year#1\time#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-100-LSMme2diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\year#1\time#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\year#1\time#1\Diff#1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme2diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\year#1\time#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\year#1\time#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='residence-SAS-100-LSMme2diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='residence-SAS-100-LSMme2jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\year#1\time#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\year#1\time#1\LSMLines#1;
   replay \GLM#1\LSMEANS#3\year#1\time#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='residence-SAS-100-diagnostic' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\time#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed */

ods tagsets.mycolorlatex file='residence-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'year')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'year')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')=0 and index(lowcase(effect),'year')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'year')>0;
   var sex year _sex _year estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'year')=0;
   var sex year _sex _year estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')=0 and index(lowcase(effect),'year')>0;
   var sex year _sex _year estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='residence-SAS-200-LSMintjline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   replay \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme1jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup2#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup2#1\Print#1;
   replay \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='residence-SAS-200-LSMme2jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup3#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup3#1\Print#1;
   replay \Print#1\ByGroup3#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='residence-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
