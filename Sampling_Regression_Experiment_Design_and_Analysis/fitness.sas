/*  Estimate the probability of being a female given other attributes
    This is the fitness.jmp data file that ships with JMP.

    Linneruds Fitness data, fitting oxygen uptake to exercise and other variables. 
    The original is in Rawlings (1988), but certain values of MaxPulse and RunPulse were 
    changed for illustration. Names and Sex columns were contrived for illustration. */

/* 2015-07-06 CJS First Edition */

/* Logistic regression */ 
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;

title 'Predicting sex as a function of various variables';
ods pdf file='fitness-SAS.pdf' style=styles.printer; 

*---part001b;
data fitness;
   infile 'fitness.csv' dlm=',' dsd missover firstobs=2;
   length name $10 sex $1;
   input Name Sex Age Weight  Oxy Runtime RunPulse RstPulse MaxPulse; 
   phat = 0;
   if sex = 'F' then phat=1;  /* create 0/1 variable for plotting */
run;
*---part001e;

proc print data=fitness;
   title2 'raw data';
run;

 
ods document name=genmod1(write);
*---partgenmodb;
/********* using GENMOD ********/
proc genmod data=fitness plot=all;
   title2 'fit a generalized linear model';
   model sex = age weight oxy runtime / dist=binomial link=logit type3;
   output out=genmod_fit p=pred l=lower u=upper resdev=resdev;
   contrast 'drop age/runtime' age 1, runtime 1;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
   ods output contrasts         =genmodcontrast;
run;
*---partgenmode;
ods document close;


ods document name=genmodrs(write);
*---partglmrsb;
/********* Fitting the response surface ********/
proc genmod data=fitness plots=all;
   title2 'fit a generalized linear model';
   model sex = weight oxy weight*weight oxy*oxy weight*oxy / dist=binomial link=logit type3;
   ods output parameterestimates=genmodrsest;
   ods output type3             =genmodrstest;
run;
*---partglmrse;
ods document close;


ods document name=genmodred(write);
*---partglmredb;
/********* Fitting the simpler model ********/
proc genmod data=fitness plots=all;
   title2 'fit a generalized linear model';
   model sex = weight oxy  / dist=binomial link=logit type3;
   ods output parameterestimates=genmodredest;
   ods output type3             =genmodredtest;
run;
*---partglmrede;
ods document close;




ods pdf close;

/* now create the LaTeX files for inclusion in the course notes */
%include "../../MyLatexTagset.sas"; run;
ods listing close;
title;
footnote;
ods listing;

proc document name=genmodred;
 list /levels=all;
run;

ods tagsets.mylatex file='fitness-SAS-data.tex' (notop nobot);
proc print data=fitness(obs=10);
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='fitness-SAS-genmodest.tex' (notop nobot);
proc print data=genmodest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fitness-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fitness-SAS-genmodcontrast.tex' (notop nobot);
proc print data=genmodcontrast noobs label split=" ";
run;
ods tagsets.mylatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="fitness-SAS-genmodplot" reset=index;
proc document name=plot3;
  replay \sgplot#1\sgplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* response surface that fails*/
ods tagsets.mylatex file='fitness-SAS-genmodrsest.tex' (notop nobot);
proc print data=genmodrsest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fitness-SAS-genmodrstest.tex' (notop nobot);
proc print data=genmodrstest noobs label split=" ";
run;
ods tagsets.mylatex close;



/* reduced model estimates and term tests */
ods tagsets.mylatex file='fitness-SAS-genmodredest.tex' (notop nobot);
proc print data=genmodredest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fitness-SAS-genmodredtest.tex' (notop nobot);
proc print data=genmodredtest noobs label split=" ";
run;
ods tagsets.mylatex close;


/* diagnostic plots */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="fitness-SAS-genmodreddiag1" reset=index;
proc document name=genmodred;
  replay \Genmod#1\DiagnosticPlot#1\DiagnosticPlot#1/ dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="fitness-SAS-genmodreddiag2" reset=index;
proc document name=genmodred;
  replay \Genmod#1\DiagnosticPlot#1\DiagnosticPlot#2/ dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="fitness-SAS-genmodreddiag3" reset=index;
proc document name=genmodred;
  replay \Genmod#1\ResidualXBetaPlot#1\DiagnosticPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

