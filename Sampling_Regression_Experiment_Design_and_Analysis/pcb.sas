/* Effect of sex and species upon chemical uptake}

Several persistent chemicals accumulate up the food chain. 
Different species may vary in the amount 
of chemicals accumulated because of different prey availability
or other factors.  Because of different behavior, 
the amount may also vary by sex.

A survey was conducted to investigate how the amount of PCBs varied
among three different species of fish in Nunavut (the new Canadian 
territory just to east of the Restofit and just north of Ulofit).
Samples were taken from four fish of each sex and species and the PCB
levels (PPM) measured in the livers. */ 


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;
title 'Two factor, fixed effects, CRD - PCB levels in fish';

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='pcb-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data pcb; /* read in the raw data */
   infile 'pcb.csv' dlm=',' dsd missover firstobs=2;
   length sex $10. species $10. trt $15.;
   input pcb sex $ species $;
   trt = compbl(sex || "-" || species);  /* create a pseudo factor */
run;
*---part001e;

proc print data=pcb;
   title2 'raw data';
run;
 

ods document name=dotplot(write);
*---part010b; 
proc sgplot data=pcb;
   title2 'Side-by-side dot plots';
   yaxis label='PCB'        offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=PCB  /  markerattrs=(symbol=circlefilled);
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Find some simple summary statistics.*/ 
ods document name=meanstable(write);
*---part020b;
proc tabulate data=pcb; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class sex species;
   var pcb;
   table sex*species, pcb*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;
*---part020e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a crd, it is easy to create the se and 95% for each mean */
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part030b;
proc sort data=pcb;
   by sex species;
run;

proc means data=pcb noprint;  /* find simple summary statistics */
   by sex species;
   var pcb;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=species / group=sex;
   highlow x=species high=uclm low=lclm  / group=sex;
   yaxis label='pcb'      offsetmin=.05 offsetmax=.05;
   xaxis label='Species'  offsetmin=.05 offsetmax=.05;
run;
*---part030e;
ods document close;



 
/* Plot the log(std dev) vs. the log(mean) to see if transformation needed
   under Taylor's Power Law */
data means;
   set means;
   log_mean = log(mean);
   log_stddev = log(stddev);
run;
proc sgplot data=means;
   title2 'Look for relationship between std dev and mean (Taylors power law)';
   scatter x=log_mean  y=log_stddev  /  markerattrs=(symbol=circlefilled);
   yaxis label='log(std deviation)' offsetmin=.05 offsetmax=.05;
   xaxis label='log(mean)'          offsetmin=.05 offsetmax=.05;
run;


 


/*----------------------------- */
/* Now for the anova - never use Proc Anova - it won't work with unbalanced data */

ods document name=glm1(write); 
*---part100b;
ods graphics on; 
proc glm data=pcb plots=all;
   title2 'Anova - balanced';
   class sex species;  /* class statement identifies factors */
   model pcb = sex species sex*species ;
   lsmeans sex*species / cl stderr pdiff adjust=tukey lines;
   lsmeans sex         / cl stderr pdiff adjust=tukey lines;
   lsmeans species     / cl stderr pdiff adjust=tukey lines;
run; 
ods graphics off;
*---part100e;
ods document close;


/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=pcb plots=all;
   title2 'Mixed m- balanced';
   class sex species;  /* class statement identifies factors */
   model pcb = sex species sex*species / ddfm=kr;
   lsmeans sex*species /  cl adjust=tukey ;
   lsmeans sex         /  cl adjust=tukey ;
   lsmeans species     /  cl adjust=tukey ;
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

 

/******************* REMOVE SOME OBSERVATIONS to make the design unbalanced ****/

data pcb; 
   /* create some missing values */
   set pcb;
   if sex= 'm' and species='sp1' and pcb < 21 then delete;
   if sex= 'f' and species='sp3' and pcb > 14 then delete;
run;

/* Now for the analysis with missing values */

proc tabulate data=pcb; /* proc tabulate is not for the faint of heart */
   title2 'Another summary table - unbalanced data';
   class sex species;
   var pcb;
   table sex*species, pcb*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;

ods graphics on;
proc glm data=pcb plots=all;
   title2 'Anova - unbalanced';
   class sex species;  /* class statement identifies factors */
   model pcb = sex species sex*species ;
   lsmeans sex*species / cl stderr pdiff adjust=tukey lines;
   lsmeans sex         / cl stderr pdiff adjust=tukey lines;
   lsmeans species     / cl stderr pdiff adjust=tukey lines;
run; 
ods graphics off;

ods graphics on;
proc mixed data=pcb plots=all;
   title2 'Mixed - unbalanced';
   class sex species;  /* class statement identifies factors */
   model pcb = sex species sex*species / ddfm=kr;
   lsmeans sex*species /  cl adjust=tukey ;
   lsmeans sex         /  cl adjust=tukey ;
   lsmeans species     /  cl adjust=tukey ;
   ods output tests3 =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
 

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

ods tagsets.mycolorlatex file='pcb-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=pcb(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='pcb-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Output from GLM */
ods tagsets.mycolorlatex file='pcb-SAS-100-overall.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\pcb#1\OverallANOVA#1; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\pcb#1\OverallANOVA#1;
   replay \GLM#1\ANOVA#1\pcb#1\OverallANOVA#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-type3.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\ANOVA#1\pcb#1\ModelANOVA#2; /* kill titles */
   obtitle  \GLM#1\ANOVA#1\pcb#1\ModelANOVA#2;
   replay \GLM#1\ANOVA#1\pcb#1\ModelANOVA#2;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMint.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeans#1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMintci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-100-LSMintdiff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_species#1\pcb#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_species#1\pcb#1\Diff#1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMintdiffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-SAS-100-LSMintdiffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMintjline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMLines#1;
   replay \GLM#1\LSMEANS#1\sex_species#1\pcb#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme1.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeans#1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme1ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme1diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\pcb#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\pcb#1\Diff#1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme1diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-SAS-100-LSMme1diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme1jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMLines#1;
   replay \GLM#1\LSMEANS#2\sex#1\pcb#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;








ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme2.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeans#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeans#1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeans#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme2ci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanCL#1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanCL#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme2diff.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\species#1\pcb#1\Diff#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\species#1\pcb#1\Diff#1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\Diff#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme2diffci.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanDiffCL#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanDiffCL#1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\LSMeanDiffCL#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-SAS-100-LSMme2diffplot' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='pcb-SAS-100-LSMme2jline.tex' (notop nobot);
proc document name=glm1;
   obstitle \GLM#1\LSMEANS#3\species#1\pcb#1\LSMLines#1; /* kill titles */
   obtitle  \GLM#1\LSMEANS#3\species#1\pcb#1\LSMLines#1;
   replay \GLM#1\LSMEANS#3\species#1\pcb#1\LSMLines#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='pcb-SAS-100-diagnostic' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\pcb#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed */

ods tagsets.mycolorlatex file='pcb-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'species')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'species')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'sex')=0 and index(lowcase(effect),'species')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'species')>0;
   var sex species _sex _species estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')>0 and index(lowcase(effect),'species')=0;
   var sex species _sex _species estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'sex')=0 and index(lowcase(effect),'species')>0;
   var sex species _sex _species estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-SAS-200-LSMintjline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   replay \Print#1\ByGroup1#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme1jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup2#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup2#1\Print#1;
   replay \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='pcb-SAS-200-LSMme2jline.tex' (notop nobot);
proc document name=mixed;
   obstitle \Print#1\ByGroup3#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup3#1\Print#1;
   replay \Print#1\ByGroup3#1\Print#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='pcb-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
