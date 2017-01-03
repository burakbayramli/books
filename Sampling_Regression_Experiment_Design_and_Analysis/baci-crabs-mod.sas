/* BACI design with multiple controls; 2 factor; interaction; */

/* A BACI design was used to assess the impact 
   of cooling water discharge on the density of 
   shore crabs. 

   The beach near the outlet of the cooling water 
   was sampled using several quadrats
   before and after the plant started operation. 
   Two control beaches at other locations
   in the inlet were also sampled. 

*/



/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='baci-crabs-mod-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'BACI - Shore crab density near power plan - multiple sites, one year before/after ';


*---part001b;
data crabs;
   infile 'baci-crabs-mod.csv' dlm=',' dsd missover firstobs=2;
   length SiteClass Site Period quadrat $10 trt $20;
   input SiteClass site Period quadrat density;
   trt = compress(SiteClass || "." || Site || "." || Period);
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
   by SiteClass Site descending Period;
run;

proc means data=crabs noprint;  /* find simple summary statistics */
   by SiteClass Site descending Period;
   var density;
   output out=means n=n mean=mean std=stddev stderr=stderr lclm=lclm uclm=uclm;
run;

proc sgplot data=means;
   title2 'Profile plot with 95% ci on each mean';
   series y=mean x=Period / group=Site;
   highlow x=Period high=uclm low=lclm  / group=Site;
   yaxis label='Density'    offsetmin=.05 offsetmax=.05;
   xaxis label='Period'     offsetmin=.05 offsetmax=.05 discreteorder=data;
run;
*---part030e;
ods document close;

/* ------------------------------------------------------------------------- */
/* Analysis on average of the quadrats as a paired design */
/* Compute the average */
*---part100b;
proc sort data=crabs;
   by SiteClass Site Period;
run;
proc means data=crabs noprint;
   by SiteClass Site Period;
   var density;
   output out=meancrabs mean=mean_density;
run;
proc print data=meancrabs;
run;
*---part100e;
 
/* transpose to get before/after on the same record */
*---part101b;
proc transpose data=meancrabs out=TransMeanCrabs;
   by SiteClass Site;
   var mean_density;
   id Period;
run;
data TransMeanCrabs;
   set TransMeanCrabs;
   diff = After-Before;
run;
proc print data=TransMeanCrabs;
run;
*---part101e;

/* Now for a t-test on the differences */
ods document name=TTestDiff(write);
*---part105b;
ods graphics on;
proc ttest data=TransMeanCrabs plot=all dist=normal;
   title2 'T-test on the difference (After-Before)';
   class SiteClass;
   var diff;
   ods output ttests = TtestTest;
   ods output ConfLimits=TtestCL;
   ods output Statistics=TtestStat;

run;
ods graphics on;
*---part105e;
ods document close;



/* Analysis of the averages using Proc Mixed. You could also use GLM
   but GLM won't be helpful when you have multipe sites etc */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=meancrabs plots=all;
   title2 'Mixed BACI analysis on averages';
   class SiteClass Period site;  /* class statement identifies factors */
   model Mean_density = SiteClass Period SiteClass*Period / ddfm=kr;
   random site;
   lsmeans SiteClass /  cl adjust=tukey ;
   lsmeans Period    /  cl adjust=tukey ;
   lsmeans SiteClass*Period   /  cl adjust=tukey ;
   estimate 'BACI effect' SiteClass*Period 1 -1 -1 1 / cl; 
   ods output tests3   =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans  =MixedLsmeans;
   ods output diffs    =MixedDiffs;
   ods output estimates=MixedEsts;
   ods output covparms =MixedCovParms;
run; 
ods graphics off;
*---part200e;
ods document close;



/* Analysis of the raw data using Proc Mixed. */
ods document name=mixed2(write);
*---part300b;
ods graphics on;
proc mixed data=crabs plots=all;
   title2 'Mixed BACI analysis on raw data';
   class SiteClass Period site;  /* class statement identifies factors */
   model density = SiteClass Period SiteClass*Period / ddfm=kr;
   random site site*period;
   lsmeans SiteClass /  cl adjust=tukey ;
   lsmeans Period    /  cl adjust=tukey ;
   lsmeans SiteClass*Period   /  cl adjust=tukey ;
   estimate 'BACI effect' SiteClass*Period 1 -1 -1 1 / cl; 
   ods output tests3   =Mixed2Test;  /* needed for the pdmix800 */
   ods output lsmeans  =Mixed2Lsmeans;
   ods output diffs    =Mixed2Diffs;
   ods output estimates=Mixed2Ests;
   ods output covparms =Mixed2CovParms;
run; 
ods graphics off;
*---part300e;
ods document close;

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


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=crabs(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-crabs-mod-SAS-010' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-020.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   replay   \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-crabs-mod-SAS-030' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-100.tex' (notop nobot);
proc print data=meancrabs(drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-101.tex' (notop nobot);
proc print data=TransMeanCrabs;
run;
ods tagsets.mycolorlatex close;



/* Output from Proc Ttest on differences */

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-105a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-105b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-105c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-crabs-mod-SAS-105d' reset=index;
proc document name=ttestdiff;
   replay \Ttest#1\diff#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;





/* Output from Proc Mixed from the analysis on the averages  */
ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-vc.tex' (notop nobot);
proc print data=MixedCovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-200-BACIeff.tex' (notop nobot);
proc print data=MixedEsts noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-crabs-mod-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;



/* Output from Proc Mixed from the analysis on the individual values  */
ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-type3.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-vc.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMint.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMme1.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMme2.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMintdiff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMme1diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-LSMme2diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-crabs-mod-SAS-300-BACIeff.tex' (notop nobot);
proc print data=Mixed2Ests noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-crabs-mod-SAS-300-diagnostic' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;










