/* Sexual activity and the lifespan of male fruitflies.
   Refer to http://www.amstat.org/publications/jse/v2n1/datasets.hanley.html
   for details on experiment and analysis.

   Briefly:

   Sexual activity was manipulated by supplying individual males with 
   one or eight receptive virgin females per day. The longevity of these males was
   compared with that of two control types. The first control consisted of two sets of individual
   males kept with one or eight newly inseminated females. 
   Newly inseminated females will not usually remate for at least two
   days, and thus served as a control for any effect of competition with the male 
   for food or space. The second control was a set of individual males 
   kept with no females. There were 25 males in each of the five groups, 
   which were treated identically in number of anaesthetizations (using CO2) and provision of fresh food medium. */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate noovp orientation=landscape;
ods pdf file='fruitfly-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Sexual activity and the lifespan of male fruitflies';
 
*---part001b;
data fruitfly;
   infile 'fruitfly.csv' dlm=',' dsd missover firstobs=2;
   input flyid partners type longevity thorax sleep;
   length group $3;
   if type = 9 then group = 'g00';
   if type = 0 and partners = 1 then group = '1p';
   if type = 0 and partners = 8 then group = '8p';
   if type = 1 and partners = 1 then group = '1v';
   if type = 1 and partners = 8 then group = '8v';
run;
*---part001e;

proc print data=fruitfly(obs=20);
   title2 'part of the raw data';
run;


ods document name=plot1(write);
*---part005b;
proc sgplot data=fruitfly;
   title2 'dot plot of rawdata';
   scatter x=group y=longevity;
run;
*---part005e;
ods document close;

*---part010b; 
proc tabulate data=fruitfly;
   title2 'summary statistics';
   class group type partners;
   var longevity;
   table group*type*partners, longevity*(n*f=5.0 mean*f=7.1 std*f=7.1) /rts=20;
run;
*---part010e;
  
 
/* plot some side-by-side box and dot  plots */

proc gplot data=fruitfly;
   title2 'a side-by-side dot plot';
   axis1 label=(a=90 r=0 'Longevity (days)') ;
   axis2 label=('Group')  offset=(1 cm, 1 cm);
   plot longevity*group / vaxis=axis1 haxis=axis2;
   symbol1 v=dot i=std2jm;  
   footnote 'Individual points and 95% confidence intervals for mean';
run;

/* create side-by-side box plots */
proc sort data=fruitfly; by group;
proc boxplot data=fruitfly;
   title2 'side-by-side box plots';
   plot longevity*group / BOXSTYLE=SCHEMATICID;
   id flyid;
   footnote ' ';
run;

/* create histograms of the groups */
proc gchart data=fruitfly;
   title2 'histograms of the longevitys';
   vbar longevity / midpoints= 10 to 100  by 20 type=percent group=group space=0;
run;
   


 
ods document name=GLM(write);
*---part030b;
ods graphics on;
proc glm data=fruitfly plots=all;
   title2 'ANOVA using GLM';
   class group;
   model longevity = group;
   lsmeans group / adjust=tukey pdiff cl stderr lines;
   ods output LSmeanDiffCL = GLMdiffs;
   ods output LSmeans      = GLMLSmeans;
   ods output LSmeanCL     = GLMLSmeansCL;
   ods output LSMlines     = GLMlines;
   ods output ModelANOVA   = GLManova;
run;
ods graphics off;
*---part030e;
ods document close;


/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part040b;
ods graphics on;
proc mixed data=fruitfly plots=all;
   title2 'ANOVA using Mixed';
   class group;
   model longevity=group;
   lsmeans group / adjust=tukey diff cl;
   ods output tests3 =MixedTest; 
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
run;
ods graphics off;
*---part040e;
ods document close;

/* Get a joined lines plot */
*---part045b;
%include 'pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
*---part045e;

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

ods tagsets.mycolorlatex file='fruitfly-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fruitfly(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fruitfly-SAS-005' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='fruitfly-SAS-010.tex' (notop nobot);
proc tabulate data=fruitfly;
   title2 ' ';
   class group type partners;
   var longevity;
   table group*type*partners, longevity*(n*f=5.0 mean*f=7.1 std*f=7.1) /rts=20;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fruitfly-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fruitfly-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='fruitfly-SAS-030bb.tex' (notop nobot);
proc print data=GLMLSmeansCL noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fruitfly-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fruitfly-SAS-030d.tex' (notop nobot);
proc print data=GLMlines noobs label split=" ";
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fruitfly-SAS-030e' reset=index;
proc document name=glm;
   replay \GLM#1\LSMEANS#1\group#1\longevity#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fruitfly-SAS-030f' reset=index;
proc document name=glm;
   replay \GLM#1\ANOVA#1\longevity#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='fruitfly-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fruitfly-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fruitfly-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var group _group estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fruitfly-SAS-040f' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='fruitfly-SAS-045.tex' (notop nobot);
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;
