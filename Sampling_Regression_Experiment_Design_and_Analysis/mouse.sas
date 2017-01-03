/* An example of a two factor experiment analyzed in SAS  - unbalance data*/
 
/* 
French (1976, Selection of high temperature for 
hibernation by the pocket mouse: Ecological advantages
and energetic consequences, Ecology, 57, 185-191)
collected the following data on the energy utilization of the pocket mouse 
(Perognathus longimembris}) during
hibernation at different temperatures: 
All readings are in kcal/g.  */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='mouse-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Two factor, fixed effects, unbalanced, CRD - energy requirement of pocket mice';

*---part001b;
data energy;
   infile 'mouse.csv' dlm=',' dsd missover firstobs=2;
   length diet $10 temp $3 trt $15;
   input temp diet energy;
   substr(trt,1,3) = temp;
   substr(trt,4,1) = '-';
   substr(trt,5  ) = diet;
run;
*---part001e;

proc print data=energy;
   title2 'raw data';
run;


ods document name=dotplot(write);
*---part010b; 
proc sgplot data=energy;
   title2 'Side-by-side dot plots';
   yaxis label='Enercy'     offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=energy  /  markerattrs=(symbol=circlefilled);
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Find some simple summary statistics.*/ 
ods document name=meanstable(write);
*---part020b;
proc tabulate data=energy; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class temp diet;
   var energy;
   table temp*diet, energy*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;
*---part020e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a crd, it is easy to create the se and 95% for each mean */
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part030b;
proc sort data=energy;
   by temp diet;
run;

proc means data=energy noprint;  /* find simple summary statistics */
   by temp diet;
   var energy;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=diet / group=temp;
   highlow x=diet high=uclm low=lclm  / group=temp;
   yaxis label='Energy'  offsetmin=.05 offsetmax=.05;
   xaxis label='Diet'    offsetmin=.05 offsetmax=.05;
run;
*---part030e;
ods document close;



/*----------------------------- */
/* Now for the anova - never use proc anova - it won't work in unbalanced data */

ods document name=glm1(write); 
*---part100b;
ods graphics on;  
proc glm data=energy plots=all;
   title2 'Anova using GLM';
   class temp diet;  /* class statement identifies factors */
   model energy = temp diet temp*diet;
   /* Note that the type III ss do not add to model ss because 
      of imbalance in the design */
   lsmeans temp*diet /cl stderr pdiff adjust=tukey lines;
   lsmeans temp /     cl stderr pdiff adjust=tukey lines;
   lsmeans diet /     cl stderr pdiff adjust=tukey lines;
run; 
ods graphics off;
*---part100e;
ods document close;


/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=energy plots=all;
   title2 'Mixed analysis';
   class temp diet;  /* class statement identifies factors */
   model energy = temp diet temp*diet / ddfm=kr;
   lsmeans temp*diet /  cl adjust=tukey ;
   lsmeans temp         /  cl adjust=tukey ;
   lsmeans diet     /  cl adjust=tukey ;
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

proc document name=mixed;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='mouse-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=energy(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mouse-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='mouse-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mouse-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Output from GLM */
ods tagsets.mycolorlatex file='mouse-SAS-100-overall.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\energy#1\OverallANOVA#1; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\energy#1\OverallANOVA#1;
   replay \GLM#1\ANOVA#1\energy#1\OverallANOVA#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-type3.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\energy#1\ModelANOVA#2; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\energy#1\ModelANOVA#2;
   replay \GLM#1\ANOVA#1\energy#1\ModelANOVA#2;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMint.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeans#1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMintci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-100-LSMintdiff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\temp_diet#1\energy#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\temp_diet#1\energy#1\Diff#1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMintdiffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mouse-SAS-100-LSMintdiffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMintjline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMLines#1;
   replay \GLM#1\LSMEANS#1\temp_diet#1\energy#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme1.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeans#1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme1ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme1diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\temp#1\energy#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\temp#1\energy#1\Diff#1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme1diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mouse-SAS-100-LSMme1diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme1jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\temp#1\energy#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\temp#1\energy#1\LSMLines#1;
   replay \GLM#1\LSMEANS#2\temp#1\energy#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;








ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme2.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeans#1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme2ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme2diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\diet#1\energy#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\diet#1\energy#1\Diff#1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme2diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mouse-SAS-100-LSMme2diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='mouse-SAS-100-LSMme2jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\diet#1\energy#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\diet#1\energy#1\LSMLines#1;
   replay \GLM#1\LSMEANS#3\diet#1\energy#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='mouse-SAS-100-diagnostic' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\energy#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed */

ods tagsets.mycolorlatex file='mouse-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'temp')>0 and index(lowcase(effect),'diet')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'temp')>0 and index(lowcase(effect),'diet')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'temp')=0 and index(lowcase(effect),'diet')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'temp')>0 and index(lowcase(effect),'diet')>0;
   var temp diet _temp _diet estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'temp')>0 and index(lowcase(effect),'diet')=0;
   var temp diet _temp _diet estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'temp')=0 and index(lowcase(effect),'diet')>0;
   var temp diet _temp _diet estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mouse-SAS-200-LSMintjline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   replay \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme1jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup2#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup2#1\Print#1;
   replay \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='mouse-SAS-200-LSMme2jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup3#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup3#1\Print#1;
   replay \Print#1\ByGroup3#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='mouse-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
