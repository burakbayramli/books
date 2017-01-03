/* Single factor CRD - 2 levels */

/* Compare the DDT levels between males and females */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


options nodate noovp orientation=landscape;
ods pdf file='ddt2g-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
title 'Effect of growth ddt2g on mean ddt of cattle';

*---part001b; 
data ddt2g;
   infile 'ddt2g.csv' dlm=',' dsd missover firstobs=2;
   input sex $ ddt;
run;
*---part001e;

proc print data=ddt2g;
   title2 'raw data';
run;
  


/* Dot plot of the two sex's ddts */
ods document name=plot1(write);
*---part002b;
proc sgplot data=ddt2g;
   title2 'Plot of ddt vs. sex';
   scatter x=sex y=ddt;
   xaxis offsetmin=.05 offsetmax=.05;
run;
*---part002e;
ods document close;


ods document name=plot2(write);
*---part005b;
proc sgplot data=ddt2g;
   title2 'Box plots';
   vbox ddt / group=sex notches;
   /* the notches options creates overlap region to compare if medians are equal */
run;
*---part005e;
ods document close;

 
*---part010b;
proc tabulate data=ddt2g;
   title2 'some basic summary statistics';
   class sex;
   var ddt;
   table sex, ddt*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
*---part010e;





ods document name=ttest1(write);
*---part020b;
ods graphics on;
proc ttest data=ddt2g plot=all dist=normal;
   title2 'test of equality of ddts between the two sexs';
   class sex;
   var ddt;
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
proc glm data=ddt2g plots=all;
   title2 'Analysis using GLM';
   class sex;
   model ddt=sex;
   lsmeans sex / pdiff stderr cl;
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
proc mixed data=ddt2g plots=all;
   title3 'analysis using Mixed ';
   class sex;
   model ddt = sex;  
   lsmeans sex / cl diff;
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

ods tagsets.mycolorlatex file='ddt2g-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=ddt2g(obs=10);
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-005' reset=index;
proc document name=plot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='ddt2g-SAS-010.tex' (notop nobot);
proc tabulate data=ddt2g;
   class sex;
   var ddt;
   table sex, ddt*(n*f=5.0 mean*f=5.1 std*f=5.1 stderr*f=7.2  lclm*f=7.1 uclm*f=7.1) / rts=20;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-020a.tex' (notop nobot);
proc print data=TtestTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-020b.tex' (notop nobot);
proc print data=TtestCL noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-020c.tex' (notop nobot);
proc print data=TtestStat noobs label split=" " ;
   var variable class n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-020d' reset=index;
proc document name=ttest1;
   replay \Ttest#1\ddt#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;





ods tagsets.mycolorlatex file='ddt2g-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-030e' reset=index;
proc document name=glm1;
   replay \GLM#1\LSMEANS#1\sex#1\ddt#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-030f' reset=index;
proc document name=glm1;
   replay \GLM#1\ANOVA#1\ddt#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;







ods tagsets.mycolorlatex file='ddt2g-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='ddt2g-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='ddt2g-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='ddt2g-SAS-040f' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;



