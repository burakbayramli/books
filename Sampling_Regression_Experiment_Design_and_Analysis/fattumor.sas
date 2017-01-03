/* Single Factor CRD; Two-sample t-test */

/* Recent epidemiological studies have shown that people who consume high fat
   diets have higher cancer rates and more severe cancers than low fat diets.

   Rats were randomized to one of two diets, one low in fat and the other high
   in fat. [Why was randomization done?] At the end of the study, the rats were
   sacrificed, the tumors excised, and the weight of the tumors found. */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

options nodate  noovp orientation=landscape;
ods pdf file='fattumor-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Tumor weight and fat levels';
 
*---part001b;
data weight;
   length diet $10.;
   infile 'fattumor.csv' dlm=',' dsd missover firstobs=2;
   input diet $ weight;
run;
*---part001e;

proc print data=weight;
   title2 'raw data';
run;
  


/* Dot plot of the two diet's weights */
ods document name=plot1(write);
*---part002b;
proc sgplot data=weight;
   title2 'Plot of weight vs. diet';
   scatter x=diet y=weight;
   xaxis offsetmin=.05 offsetmax=.05;
run;
*---part002e;
ods document close;


ods document name=plot2(write);
*---part005b;
proc sgplot data=weight;
   title2 'Box plots';
   vbox weight / group=diet notches;
   /* the notches options creates overlap region to compare if medians are equal */
run;
*---part005e;
ods document close;

 
*---part010b;
proc tabulate data=weight;
   title2 'some basic summary statistics';
   class diet;
   var weight;
   table diet, weight*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
*---part010e;





ods document name=ttest1(write);
*---part020b;
ods graphics on;
proc ttest data=weight plot=all dist=normal;
   title2 'test of equality of weights between the two diets';
   class diet;
   var weight;
   ods output ttests = TtestTest;
   ods output ConfLimits=TtestCL;
   ods output Statistics=TtestStat;
run;
ods graphics off;
*---part020e;
ods document close;


ods document name=glm1(write);
*---part030b;
ods graphics on;
proc glm data=weight plots=all;
   title2 'Analysis using GLM';
   class diet;
   model weight=diet;
   lsmeans diet / pdiff stderr cl;
   ods output LSmeanDiffCL = GLMdiffs;
   ods output LSmeans      = GLMLSmeans;
   ods output ModelANOVA   = GLManova;
   footnote 'This ie equivalent to the equal variance t-test';
run;
ods graphics off;
*---part030e;

 
ods document name=mixed1(write);
***part040b;
ods graphics on;
proc mixed data=weight plots=all;
   title3 'analysis using Mixed ';
   class diet;
   model weight = diet;  
   lsmeans diet / cl diff;
   ods output tests3  =MixedTest; 
   ods output lsmeans =MixedLsmeans;
   ods output diffs   =MixedDiffs;
   footnote 'This ie equivalent to the equal variance t-test';
run;
ods graphics off;
*---part040e;
ods document close;
run;

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

ods tagsets.mycolorlatex file='fattumor-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=weight(obs=10);
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-005' reset=index;
proc document name=plot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='fattumor-SAS-010.tex' (notop nobot);
proc tabulate data=weight;
   class diet;
   var weight;
   table diet, weight*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-020a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-020b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-020c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-020d' reset=index;
proc document name=ttest1;
   replay \Ttest#1\weight#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;





ods tagsets.mycolorlatex file='fattumor-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-030e' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\diet#1\weight#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-030f' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\weight#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;







ods tagsets.mycolorlatex file='fattumor-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fattumor-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fattumor-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fattumor-SAS-040f' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
