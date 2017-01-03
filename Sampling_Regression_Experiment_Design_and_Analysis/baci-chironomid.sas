/* BACI design with multiple years before/after ; 2 factor; interaction; */

/* This example comes from Krebs (1999), Ecological Methodology, 2nd Edition, Box 10.3.

   Estimates of chironomid (a type of invertebrate (an ohgochaste worm) 
   present in aquatic sediments) abundance in sediments were taken at one 
   station above and one below a pulpmill outflow pipe for 3 years before plant operation 
   and six years after the plant started.
*/


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

/* the noovp options to get the output to print properly on some printers */
options nodate noovp orientation=landscape;
ods pdf file='baci-chironomid-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'BACI - Chironomid Counts ';


*---part001b;
data chironomid;
   infile 'baci-chironomid.csv' dlm=',' dsd missover firstobs=2;
   length Period $20;
   input Year ControlCount TreatmentCount Period;
   Diff = TreatmentCount - ControlCount;
run;
*---part001e;

proc print data=chironomid;
   title2 'raw data';
run;


ods document name=timeplot(write);
*---part010b; 
proc sgplot data=chironomid;
   title2 'Plot of counts over time';
   yaxis label='Count' offsetmin=.05 offsetmax=.05;
   xaxis label='Year'  offsetmin=.05 offsetmax=.05;
   series x=Year  y=ControlCount    /  markerattrs=(symbol=circlefilled);
   series x=Year  y=TreatmentCount  /  markerattrs=(symbol=circlefilled);
   refline 1990.5 / axis=x;
run;
*---part010e; 
ods document close;


/*------------------------- */
/* Simple summary statistics are not very useful here*/ 


/* A simple profile plot is not very useful here as well */
 
/* ------------------------------------------------------------------------- */
/* Analysis of the differences 
/* transpose to get before/after on the same record */

ods document name=timeplotdiff(write);
*---part102b; 
proc sgplot data=Chironomid;
   title2 'Plot of the difference (Impact-Control) over time';
   yaxis label='Difference in Count' offsetmin=.05 offsetmax=.05;
   xaxis label='Year'   offsetmin=.05 offsetmax=.05;
   series x=Year  y=diff  /  markerattrs=(symbol=circlefilled);
   refline 1990.5 / axis=x;
run;
*---part102e; 
ods document close;


/* Now for a t-test on the differences */
ods document name=TTestDiff(write);
*---part105b;
ods graphics on;
proc ttest data=Chironomid plot=all dist=normal;
   title2 'T-test on the differences (Impact-Control)';
   class Period;
   var diff;
   ods output ttests = TtestTest;
   ods output ConfLimits=TtestCL;
   ods output Statistics=TtestStat;
run;
ods graphics on;
*---part105e;
ods document close;


/* Nonparametric test on the differences */
ods document name=NparDiff(write);
*---part107b;
ods graphics on;
proc npar1way data=Chironomid plot=all wilcoxon;
   title2 'T-test on the differences (Impact-Control)';
   class Period;
   var diff;
   ods output WilcoxonTest = WilcoxonTest;
run;
ods graphics on;
*---part107e;
ods document close;




/* Analysis of the raw data using Proc Mixed. */
/* We need to stack the data */
data StackChironomid;
   set Chironomid;
   length SiteClass $10;
   SiteClass = 'Treatment'; count=TreatmentCount; output;
   SiteClass = 'Control'  ; count=ControlCount;   output;
   keep siteclass year period count;
run;
proc print data=StackChironomid(obs=10);
   title2 'Stacked data';
run;


ods document name=mixed2(write);
*---part300b;
ods graphics on;
proc mixed data=StackChironomid plots=all;
   title2 'BACI analysis on raw data using MIXED';
   class SiteClass Period year;  /* class statement identifies factors */
   model count = SiteClass Period SiteClass*Period / ddfm=kr;
   random year;
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

proc document name=npardiff;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='baci-chironomid-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=chironomid(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-chironomid-SAS-010' reset=index;
proc document name=timeplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;





ods tagsets.mycolorlatex file='baci-chironomid-SAS-101.tex' (notop nobot);
proc print data=transchironomid(obs=10);
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='baci-chironomid-SAS-102' reset=index;
proc document name=timeplotdiff;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Output from Proc Ttest on differences */

ods tagsets.mycolorlatex file='baci-chironomid-SAS-105a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-105b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-105c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-chironomid-SAS-105d' reset=index;
proc document name=ttestdiff;
   replay \Ttest#1\diff#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;



ods tagsets.mycolorlatex file='baci-chironomid-SAS-107a.tex' (notop nobot);
proc document name=npardiff;
   obstitle \Npar1way#1\diff#1\Wilcoxon#1\WilcoxonTest#1; /* kill titles */
   obtitle  \Npar1way#1\diff#1\Wilcoxon#1\WilcoxonTest#1;
   replay   \Npar1way#1\diff#1\Wilcoxon#1\WilcoxonTest#1;
run;
ods tagsets.mycolorlatex close;




/* Output from Proc Mixed from the analysis on the individual values  */
ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-type3.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-vc.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMint.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMme1.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMme2.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMintdiff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMme1diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-LSMme2diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-chironomid-SAS-300-BACIeff.tex' (notop nobot);
proc print data=Mixed2Ests noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-chironomid-SAS-300-diagnostic' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;










