/* Simple BACI design; 2 factor; interaction; */

/* A simple BACI design was used to assess the impact 
   of cooling water discharge on the density of 
   shore crabs. The beach near the outlet of the 
   cooling water was sampled using several quadrats
   before and after the plant started operation, 
   as was a control beach on the other side of the 
   body of water.
*/



/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='baci-crabs-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Simple BACI - Shore crab density near power plan ';


*---part001b;
data crabs;
   infile 'baci-crabs.csv' dlm=',' dsd missover firstobs=2;
   length SiteClass Site Period quadrat $10 trt $20;
   input SiteClass site Period quadrat density;
   trt = compress(SiteClass || "." || Period);
run;
*---part001e;

proc print data=crabs;
   title2 'raw data';
run;


ods document name=dotplot(write);
*---part010b; 
proc sgplot data=crabs;
   title2 'Side-by-side dot plots';
   yaxis label='Density'     offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'   offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=density  /  markerattrs=(symbol=circlefilled);
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Find some simple summary statistics.*/ 
ods document name=meanstable(write);
*---part020b;
proc tabulate data=crabs; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class SiteClass Site Period;
   var Density;
   table SiteClass*Site*Period, density*(n*f=5.0  mean*f=5.2 std*f=5.2 stderr*f=5.2) /rts=15;
run;
*---part020e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a crd, it is easy to create the se and 95% for each mean */
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part030b;
proc sort data=crabs;
   by SiteClass Period;
run;

proc means data=crabs noprint;  /* find simple summary statistics */
   by SiteClass Period;
   var density;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=SiteClass / group=Period;
   highlow x=SiteClass high=uclm low=lclm  / group=Period;
   yaxis label='Density'    offsetmin=.05 offsetmax=.05;
   xaxis label='SiteClass'  offsetmin=.05 offsetmax=.05;
run;
*---part030e;
ods document close;


/* Analysis of the design using Proc Mixed. You could also use GLM
   but GLM won't be helpful when you have multipe sites etc */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=crabs plots=all;
   title2 'Mixed BACI analysis';
   class SiteClass Period;  /* class statement identifies factors */
   model density = SiteClass Period SiteClass*Period / ddfm=kr;
   lsmeans SiteClass /  cl adjust=tukey ;
   lsmeans Period    /  cl adjust=tukey ;
   lsmeans SiteClass*Period   /  cl adjust=tukey ;
   estimate 'BACI effect' SiteClass*Period 1 -1 -1 1 / cl; 
   ods output tests3   =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans  =MixedLsmeans;
   ods output diffs    =MixedDiffs;
   ods output estimates=MixedEsts;
run; 
ods graphics off;
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

ods tagsets.mycolorlatex file='baci-crabs-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=crabs(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-crabs-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay   \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-crabs-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed */
ods tagsets.mycolorlatex file='baci-crabs-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-SAS-200-BACIeff.tex' (notop nobot);
proc print data=MixedEsts noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-crabs-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;










