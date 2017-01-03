/* Illusrate how to get side-by-side confidence interval plot for a simple design */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


title 'Comparison of weight loss under different diets';
options nodate noovp orientation=landscape;
ods pdf file='druglbi-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

data lbs;
   infile 'druglbi.csv' dlm=',' dsd missover firstobs=2;
   length drug $10;
   input drug lbi lbs;
run;
 
proc print data=lbs;
   title2 'raw data';
run;
 
ods document name=glm1(write);
ods graphics on;
proc glm data=lbs;
   title2 'analysis of means';
   class drug;
   model lbs = drug;
   lsmeans drug / cl pdiff=anom;
run;
ods graphics off;
ods document close;


ods document name=boxplot1(write);
ods graphics on;
proc boxplot data=lbs;
   plot lbs*drug /
      boxstyle = schematicid
      notches;
   label lbs = 'Lbs lost';
run;
ods graphics off;
ods document close;



ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=boxplot1;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='druglbi-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=labs;
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='druglbi-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='druglbi-SAS-003' reset=index;
proc document name=boxplot1;
   replay \Boxplot#1\lbs#1\Boxplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

