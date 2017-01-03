/* Single factor CRD - 2 levels */

/* Does feeding growth hormone increase the final weight of cattle prior to market?

   Cattle were randomized to one of two groups - either a placebo
   or the group that received injections of the hormone. In this
   experiment, the sample sizes were not equal (there was a reason
   that is not important for this example).  */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


options nodate noovp orientation=landscape;
ods pdf file='hormone-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
title 'Effect of growth hormone on mean weight of cattle';

*---part001b; 
data hormone;
   infile 'hormone.csv' dlm=',' dsd missover firstobs=2;
   input hormone control;
   /* now to convert to the usual structure for analysis - stack the variables */
   trt = 'hormone'; weight = hormone; output;
   trt = 'control'; weight = control; output;
   keep trt weight;
run;
*---part001e;

proc print data=hormone;
   title2 'raw data';
run;
  


/* Dot plot of the two trt's weights */
ods document name=plot1(write);
*---part002b;
proc sgplot data=hormone;
   title2 'Plot of weight vs. trt';
   scatter x=trt y=weight;
   xaxis offsetmin=.05 offsetmax=.05;
run;
*---part002e;
ods document close;


ods document name=plot2(write);
*---part005b;
proc sgplot data=hormone;
   title2 'Box plots';
   vbox weight / group=trt notches;
   /* the notches options creates overlap region to compare if medians are equal */
run;
*---part005e;
ods document close;

 
*---part010b;
proc tabulate data=hormone;
   title2 'some basic summary statistics';
   class trt;
   var weight;
   table trt, weight*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
*---part010e;





ods document name=ttest1(write);
*---part020b;
ods graphics on;
proc ttest data=hormone plot=all dist=normal;
   title2 'test of equality of weights between the two trts';
   class trt;
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
proc glm data=hormone plots=all;
   title2 'Analysis using GLM';
   class trt;
   model weight=trt;
   lsmeans trt / pdiff stderr cl;
   ods output LSmeanDiffCL = GLMdiffs;
   ods output LSmeans      = GLMLSmeans;
   ods output ModelANOVA   = GLManova;
   footnote 'This is equivalent to the equal variance t-test';
run;
ods graphics off;
*---part030e;

 
ods document name=mixed1(write);
***part040b;
ods graphics on;
proc mixed data=hormone plots=all;
   title3 'analysis using Mixed ';
   class trt;
   model weight = trt;  
   lsmeans trt / cl diff;
   ods output tests3  =MixedTest; 
   ods output lsmeans =MixedLsmeans;
   ods output diffs   =MixedDiffs;
   footnote 'This is equivalent to the equal variance t-test';
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

ods tagsets.mycolorlatex file='hormone-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=hormone(obs=10);
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-005' reset=index;
proc document name=plot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='hormone-SAS-010.tex' (notop nobot);
proc tabulate data=hormone;
   class trt;
   var weight;
   table trt, weight*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-020a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-020b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-020c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-020d' reset=index;
proc document name=ttest1;
   replay \Ttest#1\weight#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;





ods tagsets.mycolorlatex file='hormone-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-030e' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\trt#1\weight#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-030f' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\weight#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;







ods tagsets.mycolorlatex file='hormone-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='hormone-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='hormone-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='hormone-SAS-040f' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;



