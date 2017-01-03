/* An example of a two factor experiment analyzed in SAS */
/* 2014-10-31 CJS pdate for sgplot; styles; slices; missing values */
/* 
The Mirogrex terrau-sanctae is a commercial sardine like firsh
found in the Sea of Galilee. A study was conducted to determine 
the effect of light and temperature on the
gonadosomatic index (GSI), which is a measure of the growth of the ovary.
Two photoperiods -- 14 hours of light, 10 hours of dark and 9 hours of light, 
15 hours of dark -- and two temperature levels -- 16 and 27 C -- are used.
In this way, the experimenter can simulate both winter  
and summer conditions in the region.

Twenty females were collected in June. This group was randomly divided into
four subgroups - each of size 5. Each fish was placed in an individual tank,
 and received one of the four possible treatment
combinations. At the end of 3 months, the GSI was measured.  */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

options orientation=landscape;
ods pdf file='gsi-SAS.pdf' style=styles.printer;
goptions device=pdf colors=(black) rotate=landscape;

title 'Two factor, fixed, CRD - GSI of fish ';
 
*---part001b;
data gsi;  /* read in the raw data */
   infile 'gsi.csv' dlm=',' dsd missover firstobs=2;
   length temp $3. photo $3.;
   input temp $  photo $ gsi;
   length trt $15.;
   trt = temp || '-' || photo;  /* create a trt variable */
   /* create some missing values for an unbalanced analysis */
   gsi2 = gsi;
   if temp = '27C' & Photo = '09h' & gsi < 1   then gsi2 = .;
   if temp = '16C' & Photo = '14h' & gsi > 1.3 then gsi2 = .;
run;
*---part001e;

proc print data=gsi;
   title2 'raw data';
run;
 
ods document name=dotplot(write);
*---part010b; 
proc sgplot data=gsi;
   title2 'Side-by-side dot plots';
   yaxis label='GSI'        offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=GSI  /  markerattrs=(symbol=circlefilled);
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Find some simple summary statistics.*/ 
ods document name=meanstable(write);
*---part020b;
proc tabulate data=gsi; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class temp photo;
   var gsi;
   table temp*photo, gsi*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;
*---part020e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a crd, it is easy to create the se and 95% for each mean */
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part030b;
proc sort data=gsi;
   by temp photo;
run;

proc means data=gsi noprint;  /* find simple summary statistics */
   by temp photo;
   var gsi;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=temp / group=photo;
   highlow x=temp high=uclm low=lclm  / group=photo;
   yaxis label='GSI'          offsetmin=.05 offsetmax=.05;
   xaxis label='Temperature'  offsetmin=.05 offsetmax=.05;
run;
*---part030e;
ods document close;




/*----------------------------- */
/* Now for the anova - never use proc anova - it won't work with unbalanced data */

ods document name=glm1(write); 
*---part100b;
ods graphics on;
proc glm data=gsi plots=all;
   title2 'Anova';
   class photo temp;  /* class statement identifies factors */
   model gsi = photo temp photo*temp;
   /* because interaction is significant, only get trt means and do multiple comp */
   lsmeans photo*temp / cl stderr pdiff adjust=tukey lines slice=photo slice=temp;
   lsmeans photo / cl stderr pdiff adjust=tukey;
   lsmeans temp  / cl stderr pdiff adjust=tukey ; 
run;
ods graphics off;
*---part100e;
ods document close;

/* You can also do the Analysis on the separate treatments */
ods graphics on; 
proc glm data=gsi;
   title2 'ANOVA on treatment levels without factorial structure';
   class trt;
   model gsi= trt;
   lsmeans trt / cl stderr pdiff adjust=tukey lines;
run;
ods graphics off;


/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=gsi plots=all;
   title2 'ANOVA using Mixed';
   class photo temp;
   model gsi=photo temp photo*temp / ddfm=kr;
   lsmeans photo*temp / adjust=tukey diff cl slice=photo slice=temp;
   lsmeans photo      / adjust=tukey diff cl;
   lsmeans temp       / adjust=tukey diff cl;
   ods output tests3 =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
   ods output slices =MixedSlices;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
*---part200e;
ods document close;


/**************************************************************************************/
/* desl with missing values and unbalance design */


ods document name=glm5(write); 
*---part500b;
ods graphics on;
proc glm data=gsi plots=all;
   title2 'Anova - unbalanced data';
   class photo temp;  /* class statement identifies factors */
   model gsi2 = photo temp photo*temp;
   /* because interaction is significant, only get trt means and do multiple comp */
   lsmeans photo*temp / cl stderr pdiff adjust=tukey lines slice=photo slice=temp;
   lsmeans photo / cl stderr pdiff adjust=tukey;
   lsmeans temp  / cl stderr pdiff adjust=tukey ; 
run;
ods graphics off;
*---part500e;
ods document close;


/* Alternatively, you can use Mixed */
ods document name=mixed5(write);
*---part600b;
ods graphics on;
proc mixed data=gsi plots=all;
   title2 'ANOVA using Mixed - unbalanced data';
   class photo temp;
   model gsi2=photo temp photo*temp / ddfm=kr;
   lsmeans photo*temp / adjust=tukey diff cl slice=photo slice=temp;
   lsmeans photo      / adjust=tukey diff cl;
   lsmeans temp       / adjust=tukey diff cl;
   ods output tests3 =MixedTest2;  /* needed for the pdmix800 */
   ods output lsmeans=MixedLsmeans2;
   ods output diffs  =MixedDiffs2;
   ods output slices =MixedSlices2;
run;
ods graphics off;

/* Get a joined lines plot */
%pdmix800(MixedDiffs2,MixedLsmeans2,alpha=0.05,sort=yes);
*---part600e;
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

ods tagsets.mylatex file='gsi-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=GSI(obs=10);
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='gsi-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='gsi-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='gsi-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Output from GLM */
ods tagsets.mylatex file='gsi-SAS-100-overall.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\gsi#1\OverallANOVA#1; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\gsi#1\OverallANOVA#1;
   replay \GLM#1\ANOVA#1\gsi#1\OverallANOVA#1;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-100-type3.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\gsi#1\ModelANOVA#2; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\gsi#1\ModelANOVA#2;
   replay \GLM#1\ANOVA#1\gsi#1\ModelANOVA#2;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-100-LSMint.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeans#1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeans#1;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-100-LSMintci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanCL#1;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='gsi-SAS-100-LSMintdiff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\Diff#1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\Diff#1;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-100-LSMintdiffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMeanDiffCL#1;
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='gsi-SAS-100-LSMintdiffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='gsi-SAS-100-LSMintjline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMLines#1;
   replay \GLM#1\LSMEANS#1\photo_temp#1\gsi#1\LSMLines#1;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='gsi-SAS-100-diagnostic' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\gsi#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed */

ods tagsets.mylatex file='gsi-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-200-LSMint.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mylatex close;

/* This is often too big, so we use the tables with selected output */
ods tagsets.mylatex file='gsi-SAS-200-LSMintdiff.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var photo temp _photo _temp estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='gsi-SAS-200-LSMsliceTempByPhoto.tex' (notop nobot);
proc print data=MixedSlices noobs label split=' ';
   where photo ^= ' ';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='gsi-SAS-200-LSMintjline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   replay \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='gsi-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
