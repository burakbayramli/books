/* A study of height and weight measurements from 63 children, all age 12.      
  Is it safe to say at age 12, the mean height for males will be greater than for females?      

  Ths is the HTWT12 datafile in the JMP sample data library.        */

/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


options nodate  noovp orientation=landscape;
ods pdf file='htwt12-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Comparing heights of 12 year boys and girls';
 
*---part001b;
data height;
   length gender $4;
   infile 'htwt12.csv' dlm=',' dsd missover firstobs=2;
   input gender $ height weight;
run;
*---part001e;

proc print data=height (obs=10);
   title2 'part of the raw data';
run;



/* Dot plot of the two gender's heights */
ods document name=plot1(write);
*---part002b;
proc sgplot data=height;
   title2 'Plot of height vs. gender';
   scatter x=gender y=height;
   xaxis offsetmin=.05 offsetmax=.05;
run;
*---part002e;
ods document close;


ods document name=plot2(write);
*---part005b;
proc sgplot data=height;
   title2 'Box plots';
   vbox height / group=gender notches;
   /* the notches options creates overlap region to compare if medians are equal */
run;
*---part005e;
ods document close;

 
*---part010b;
proc tabulate data=height;
   title2 'some basic summary statistics';
   class gender;
   var height;
   table gender, height*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
*---part010e;



ods document name=ttest1(write);
*---part020b;
ods graphics on;
proc ttest data=height plot=all dist=normal;
   title2 'test of equality of heights between males and females';
   class gender;
   var height;
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
proc glm data=height plots=all;
   title2 'Analysis using GLM';
   class gender;
   model height=gender;
   lsmeans gender / pdiff stderr cl;
   ods output LSmeanDiffCL = GLMdiffs;
   ods output LSmeans      = GLMLSmeans;
   ods output ModelANOVA   = GLManova;
   footnote 'This is equivalent to the equal variance t-test';
run;
ods graphics off;
*---part030e;

 
ods document name=mixed1(write);
*---part040b;
ods graphics on;
proc mixed data=height plots=all;
   title3 'analysis using Mixed ';
   class gender;
   model height = gender;  
   lsmeans gender / cl diff;
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

ods tagsets.mycolorlatex file='htwt12-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=height(obs=10);
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-005' reset=index;
proc document name=plot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='htwt12-SAS-010.tex' (notop nobot);
proc tabulate data=height;
   class gender;
   var height;
   table gender, height*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-020a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-020b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-020c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-020d' reset=index;
proc document name=ttest1;
   replay \Ttest#1\height#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;





ods tagsets.mycolorlatex file='htwt12-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-030e' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\gender#1\height#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-030f' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\height#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;







ods tagsets.mycolorlatex file='htwt12-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='htwt12-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='htwt12-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='htwt12-SAS-040f' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
