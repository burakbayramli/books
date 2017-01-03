/* RCB analysis  */

/* An experiment was conducted to investigate the effect of different
   herbicides on the spike weight of gladiolus. The idea is that
   the herbicides will kill competing weeds and allow more growth
   to occur.

   Four fields were randomly selected from a set of fields
   scattered around the province at various experimental farms, 
   and each field was split into
   4 smaller fields. Four different herbicides were randomly assigned
   to the smaller plots in a RCB design.
*/
  
/* Lines starting with *---part001b; or *---part001e; bracket the source 
   line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

options nodate noovp orientation=landscape;
ods pdf file='herbicide-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Herbicide experiment - an RCB';

*---part001b;
data yield;
   length herb field $10.;
   infile 'herbicide.csv' dlm=',' dsd missover firstobs=2;
   input herb field yield;
run;
*---part001e;

proc print data=yield;
   title2 'raw data';
run;
 
ods document name=plot1(write);
*---part002b;
proc sgplot data=yield;
   title2 'preliminary plot to check additivity';
   series y=yield x=field / group=herb;
run;
*---part002e;
ods document close;

*---part003b;
proc tabulate data=yield;
   title2 'Summary of means and std dev';
   class herb;
   var yield;
   table herb, yield*(n*f=5.0 mean*f=7.2 std*f=7.2);
run;
*---part003e;


ods document name=mixed(write);
*---part020b;
ods graphics on;
proc mixed data=yield plot=all;
   title2 'Analysis using Proc Mixed';
   class field herb;          /* the two effects in the model */
   model yield = field herb / ddfm=kr;
   lsmeans herb / diff adjust=tukey; /* multiple comparisons - tukey adjustment */
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
*---part020e;
ods document close;

/* Get a joined lines plot */
*---part040b;
%include 'pdmix800.sas';
%pdmix800(diffs3,lsmeans3,alpha=0.05,sort=yes);
*---part040e;


ods pdf close;




/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=plot1;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='herbicide-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=yield;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='herbicide-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='herbicide-SAS-003.tex' (notop nobot);
proc tabulate data=yield;
   class herb;
   var yield;
   table herb, yield*(n*f=5.0 mean*f=7.2 std*f=7.2);
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='herbicide-SAS-025.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='herbicide-SAS-026.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='herbicide-SAS-027.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='herbicide-SAS-024' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='herbicide-SAS-040.tex' (notop nobot);
%pdmix800(diffs3,lsmeans3,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;



run;
 
