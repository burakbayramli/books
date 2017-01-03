/*  Example of a RCB analysis with BLOCKS as fixed effects */

/* Effect of salt on biomass# An experiment was conducted where different amounts 
   of salt (ppm) were added to plots and the resulting 
   biomass of grass was measured. The experiment was 
   replicated in four blocks. */

/* Lines starting with *---part001b; or *---part001e; bracket the source 
   line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

options nodate noovp orientation=landscape;
ods pdf file='salt-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'RCB - Fixed block effects - Salt Marsh Biomass';

*---part001b;
data biomass;
   length block $10.;
   infile 'salt.csv' dlm=',' dsd missover firstobs=2;
   input salt biomass block;
run;
*---part001e;

proc sort data=biomass;
   by block salt;
run;
 
proc print data=biomass;
   title2 'raw data';
run;


ods document name=plot1(write);
*---part002b;
proc sgplot data=biomass;
   title2 'preliminary plot to check additivity';
   series y=biomass x=salt / group=block;
run;
*---part002e;
ods document close; 


*---part003b;
proc tabulate data=biomass;
   title2 'Summary of means and std dev';
   class salt;
   var biomass;
   table salt, biomass*(n*f=5.0 mean*f=7.2 std*f=7.2);
run;
*---part003e;



ods document name=mixed(write);
*---part020b;
ods graphics on;
proc mixed data=biomass plot=all;
   title2 'Analysis using Proc Mixed';
   class salt block;          /* the two effects in the model */
   model biomass = salt block / ddfm=kr;
   lsmeans salt / diff adjust=tukey; /* multiple comparisons - tukey adjustment */
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

ods tagsets.mycolorlatex file='salt-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=biomass;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='salt-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='salt-SAS-003.tex' (notop nobot);
proc tabulate data=biomass;
   class salt;
   var biomass;
   table salt, biomass*(n*f=5.0 mean*f=7.2 std*f=7.2);
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='salt-SAS-025.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='salt-SAS-026.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='salt-SAS-027.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='salt-SAS-024' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='salt-SAS-040.tex' (notop nobot);
%pdmix800(diffs3,lsmeans3,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

