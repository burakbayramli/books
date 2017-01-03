/* BACI-fry

   This example is based (loosely) on a consulting project from an
   Independent Power Producer who was interested in monitoring the effects 
   of an in-stream hydroelectric project. 

   The response variable for the project was the minnow density at different locations in 
   the stream.

   The  monitoring design has the river divided into six segments of which 
   three are upstream of the diversion and three are downstream of the diversion. 
   In each segment, several sites  have been located where minnow fry 
   congregate. In each of the sites, minnow traps are set for various lengths of time. 

   At the end of the soaking period, the traps are removed and the number of 
   minnows are counted and classified by species.
   The counts are standardized to a common period of time to adjust for the 
   different soak-times.

   [This could be done directly in the analysis by using the 
    soak-time as a #covariate, but the soak-time data was not available.]

   An initial pre-project monitoring study was run in 2000 to 2002. 
   The  project became operational in late 2002, and post-project 
   monitoring continued in 2003 and 2004.
*/


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;


title 'BACI - Minnow Density at IPP';
options noovp orientation=landscape;

ods pdf file='baci-fry-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
/* read in the data */
*---part001b;
data fry;
   infile 'baci-fry.csv' dlm=',' dsd missover firstobs=2;
   length SiteClass $20 Site $20 Period $20.;
   input SiteClass Site Sample Year Period Fry;
   log_Fry = log(fry);
   attrib log_fry label='log(Fry)';
run;
*---part001e;

proc print data=fry(obs=10);
   title2 'part of the raw data';
run;


/* Look at relationship of standard deviation to the mean and show that 
   log(fry) transform works pretty much */
*---part005b;
proc sort data=fry;
   by year site;
run;
proc means data=fry noprint;
   by year site ;
   var fry log_fry;
   output out=mean_fry n=ncages mean=mean_fry mean_log_fry std=std_fry std_log_fry;
   id SiteClass period;
run;
*---part005e;

ods document name=stdmeanplot(write);
proc sgplot data=mean_fry;
   title2 'Examine the std to mean relationship';
   scatter y=std_fry x=mean_fry;
run;
proc sgplot data=mean_fry;
   title2 'Examine the std to mean relationship';
   scatter y=std_log_fry x=mean_log_fry;
run;
ods document close;



/* Get the profile plot over time */
ods document name=timeplot(write);
*---part010b;
proc sgplot data=mean_fry;
   title2 'Profile plot of response over time';
   series y=mean_log_fry x=year / group=site;
   refline 2002.5 / axis=x; 
run;
*---part010e;
ods document close;




*---------------------------------------------------------------------------------;
/* Do the analysis of the averages on the log scale */
/* Analysis of the averages using Proc Mixed. You could also use GLM
   but GLM won't be helpful when you have multipe sites etc */
ods document name=mixed(write);
*---part200b;
ods graphics on;
proc mixed data=mean_fry plots=all;
   title2 'BACI analysis on AVERAGES of log(fry)';
   class SiteClass year period site;
   model mean_log_fry = SiteClass period SiteClass*period / ddfm=kr;
   /* because site and year labels are unique, we don't need the
      nesting syntax of year(Period) site(SiteClass) */
   random year site;
   lsmeans SiteClass /  cl adjust=tukey ;
   lsmeans Period    /  cl adjust=tukey ;
   lsmeans SiteClass*Period   /  cl adjust=tukey ;
   estimate 'baci contrast' SiteClass*period 1 -1 -1 1 / cl;
   ods output tests3   =MixedTest;  /* needed for the pdmix800 */
   ods output lsmeans  =MixedLsmeans;
   ods output diffs    =MixedDiffs;
   ods output estimates=MixedEsts;
   ods output covparms =MixedCovParms;
run;
ods graphics off;
*---part200e;
ods document close;



*---------------------------------------------------------------------------------;
/* Do the analysis of the individual data points on the log scale */
ods document name=mixed(write);
*---part300b;
ods graphics on;
proc mixed data=fry plots=all nobound;
   title2 'BACI analysis on INDIVIDUAL of log(fry)  - unbounded variance components';
   class SiteClass year period site;
   model log_fry = SiteClass period SiteClass*period / ddfm=kr;
   random year(period) site(SiteClass) year*site(period SiteClass);
   estimate 'baci contrast' SiteClass*period 1 -1 -1 1 / cl;
   lsmeans SiteClass /  cl adjust=tukey ;
   lsmeans Period    /  cl adjust=tukey ;
   lsmeans SiteClass*Period   /  cl adjust=tukey ;
   ods output tests3   =Mixed2Test;  /* needed for the pdmix800 */
   ods output lsmeans  =Mixed2Lsmeans;
   ods output diffs    =Mixed2Diffs;
   ods output estimates=Mixed2Ests;
   ods output covparms =Mixed2CovParms;
run; 
ods graphics off;
*---part300e;
ods document close;


proc mixed data=fry plots=all;
   title2 'BACI analysis on INDIVIDUAL log(fry) scale using original data - bounded variance components';
   class SiteClass year period site;
   model log_fry = SiteClass period SiteClass*period / ddfm=kr;
   random year(period) site(SiteClass) year*site(period SiteClass);
   estimate 'baci contrast' SiteClass*period 1 -1 -1 1 / cl;
   footnote 'note that one variance component is negative';
run;

ods pdf close;

proc sort data=fry;  by year site; run;
data fry;
   set fry;
   by year site;
   if first.site then do;
      re = rannor(234234);
   end;
   log_fry = log_fry + re;
run;

proc mixed data=fry  method=type3 ;
   title2 'BACI analysis on INDIVIDUAL of log(fry)  - unbounded variance components';
   class SiteClass year period site;
   model log_fry = SiteClass period SiteClass*period ;
   random year(period) site(SiteClass) year*site(period SiteClass);
 *  random siteclass*year(period) period*site(siteclass) ;
   estimate 'baci contrast' SiteClass*period 1 -1 -1 1 / cl;
 run; 


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed2;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='baci-fry-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fry(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-005a.tex' (notop nobot) stylesheet="sas.sty";
proc print data=mean_fry(drop=_type_ _freq_ obs=10) split="_";
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='baci-fry-SAS-005b' reset=index;
proc document name=stdmeanplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='baci-fry-SAS-005c' reset=index;
proc document name=stdmeanplot;
   replay \Sgplot#2\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



ods listing;
ods graphics on / imagefmt=png imagename='baci-fry-SAS-010' reset=index;
proc document name=timeplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;





/* Output from Proc Mixed from the analysis on the averages  */
ods tagsets.mycolorlatex file='baci-fry-SAS-200-type3.tex' (notop nobot);
proc document name=mixed;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-200-vc.tex' (notop nobot);
proc print data=MixedCovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMint.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMme1.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMme2.tex' (notop nobot);
proc print data=MixedLSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMintdiff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMme1diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-200-LSMme2diff.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-fry-SAS-200-BACIeff.tex' (notop nobot);
proc print data=MixedEsts noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-fry-SAS-200-diagnostic' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;



/* Output from Proc Mixed from the analysis on the individual values  */
ods tagsets.mycolorlatex file='baci-fry-SAS-300-type3.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-300-vc.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMint.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMme1.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMme2.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMintdiff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMme1diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')>0 and index(lowcase(effect),'period')=0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='baci-fry-SAS-300-LSMme2diff.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   where index(lowcase(effect),'siteclass')=0 and index(lowcase(effect),'period')>0;
   var SiteClass Period _SiteClass _Period estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='baci-fry-SAS-300-BACIeff.tex' (notop nobot);
proc print data=Mixed2Ests noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='baci-fry-SAS-300-diagnostic' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;










