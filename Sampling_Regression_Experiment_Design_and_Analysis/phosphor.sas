/* Single Factor, Completely Randomized Design (CRD) */

/* A horticulturist is examining differences in the phosphorus content of tree leaves
   from three varieties.

   She randomly selects five trees from each variety within a large orchard, and
   takes a sample of leaves from each tree. The phosphorus content is determined
   for each tree. */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options orientation=landscape;
ods pdf file='phosphor-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Comparing phosphor levels among varieties of trees ';
options nodate noovp;

*---part001b;
data phosphor;
   infile 'phosphor.csv' dlm=',' dsd missover firstobs=2;
   input phosphor variety $;
run;
*---part001e;


proc print data=phosphor;
   title2 'raw data';
run;
 


ods document name=plot1(write);
*---part005b;
proc sgplot data=phosphor;
   title2 'dot plot of rawdata';
   scatter x=variety y=phosphor;
run;
*---part005e;
ods document close;

*---part010b; 
proc tabulate data=phosphor;
   title2 'summary statistics';
   class variety;
   var phosphor;
   table variety, phosphor*(n*f=5.0  mean*f=6.2 std*f=6.2) / rts=20;
run;
*---part010e;



ods document name=GLM(write);
*---part030b;
ods graphics on;
proc glm data=phosphor plots=all;
   title2 'ANOVA using GLM';
   class variety;
   model phosphor = variety;
   lsmeans variety / adjust=tukey pdiff cl stderr lines;
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
proc mixed data=phosphor plots=all;
   title2 'ANOVA using Mixed';
   class variety;
   model phosphor=variety;
   lsmeans variety / adjust=tukey diff cl;
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

ods tagsets.mycolorlatex file='phosphor-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=phosphor(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='phosphor-SAS-005' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='phosphor-SAS-010.tex' (notop nobot);
proc tabulate data=phosphor;
   title2 ' ';
   class variety;
   var phosphor;
   table variety, phosphor*(n*f=5.0  mean*f=6.2 std*f=6.2) / rts=20;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='phosphor-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='phosphor-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='phosphor-SAS-030bb.tex' (notop nobot);
proc print data=GLMLSmeansCL noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='phosphor-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='phosphor-SAS-030d.tex' (notop nobot);
proc print data=GLMlines noobs label split=" ";
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='phosphor-SAS-030e' reset=index;
proc document name=glm;
   replay \GLM#1\LSMEANS#1\variety#1\phosphor#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='phosphor-SAS-030f' reset=index;
proc document name=glm;
   replay \GLM#1\ANOVA#1\phosphor#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='phosphor-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='phosphor-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='phosphor-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var variety _variety estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='phosphor-SAS-040f' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='phosphor-SAS-045.tex' (notop nobot);
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;
