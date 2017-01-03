/* Estimate the probability of nest as a function of height */
/* Logistic regression */
/* 2015-07-05 CJS Update for latest version of SAS */

/* Made up data. The nest success as a function of height above the ground */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;

title 'Does height influence probability of nest success?';
ods pdf file='nestsuccess.pdf' style=styles.printer;
 
*---part001b;
data nestsuccess;
   infile 'nestsuccess.csv' dlm=',' dsd missover firstobs=2 ;
   input height Nnests Nsuccess;
   phat = Nsuccess/Nnests; 
   eprob = (Nsuccess+.5)/(Nnests+1); /* this has "less" bias */
   elogit= log(eprob/(1-eprob));
run;
*---part001e;

proc print data=nestsuccess;
   title2 'raw data';
run;

/* Plot the empirical probability and fit the ordinary least squares regression line */
ods document name=plot1(write);
*---part002b;
proc sgplot data=nestsuccess;
   title2 'Observed probability of success vs. height';
   scatter x=height y=phat;
   reg     x=height y=phat;
run;
*---part002e;
ods document close;

/* Plot the empirical logit vs height */
ods document name=plot2(write);
*---part003b;
proc sgplot data=nestsuccess;
   title2 'Empirical logit of success vs. height';
   scatter x=height y=elogit;
   reg     x=height y=elogit;
run;
*---part003e;
ods document close;





/* Fit the logiistic regression using the logistic and genmod procedures */
/* This will ue the events/trial syntax */

proc sort data=nestsuccess; 
   by height;
run;
 
ods document name=genmod1(write);
*---part050b;
/********* using GENMOD with events/trials syntax ********/
proc genmod data=nestsuccess plots=all;
   title2 'fit a generalized linear model using GENMOD';
   model Nsuccess/Nnests = height / dist=binomial link=logit obstats type3 waldci;
   output out=genmod_fit p=pred l=lower u=upper resdev=resdev;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
run;
*---part050e;
ods document close;

proc print data=genmod_fit(obs=10);
   title2 'the fitted points from genmod';
run;

ods document name=plot3(write);
/* Produce a plot of the fitted curves and confidence envelope */
proc sgplot data=genmod_fit;
   title2 'Predicted plot from GENMOD';
   band    x=height lower=lower upper=upper / transparency=0.5;
   scatter x=height y=phat  / markerattrs=(symbol=circlefilled color=black);
   series  x=height y=pred  / lineattrs  =(color=black);
run;
ods document close;

ods document name=logistic(write);
*---part001b;
proc logistic data=nestsuccess plots=all;
   title2 'fit a logistic regression using LOGISTIC';
   model Nsuccess/Nnests = height / link=logit;
   ods output ParameterEstimates = Logistic_estimates;
run;
*---part001e;
ods document close;


/************ Redo the analyses using the individual line for each response data format */
/* This format is often used when individual subjects are followed and each subject
   has the success/failure outcome.
   If each line represents one subject, you don't need a count variable.
   Here, each line represents multiple eggs, so we need a count variable which
   is specified in the FREQ statement in the procs */
/* Notice that the estimates etc are all identical, but the plots from the ods graphics
   look different than the previous case */
data nestsuccess2;
   set nestsuccess;
   length response $12.;
   response = 'Successful';     count = Nsuccess; output;
   response = 'Not Successful'; count = Nnests-Nsuccess; output;
   keep height phat response count;
run;
 
proc print data=nestsuccess2 (obs=10);
   title2 'part of data in alternate format';
run;

proc genmod data=nestsuccess2 plots=all;
   title2 'GENMOD fit to individual response data format';
   /* the default option is to model a "success" as the category that comes
      first when the response value are sorted alphabetically. In this case
      a "fledge" is a "success" */
   model response = height / dist=binomial link=logit waldci;
   freq count;  /* specify how many observations */
run;

proc logistic data=nestsuccess2 plots=all;
   title2 'LOGISTIC fit to individual response data format';
   /* You can specify which event is to be treated as a success */
   model response(event='Successful') = height / link=logit;
   freq count;
run;

ods pdf close;



/* now create the LaTeX files for inclusion in the course notes */
%include "../../MyLatexTagset.sas"; run;
ods listing close;
title;
footnote;
ods listing;

proc document name=plot2;
 list /levels=all;
run;

ods tagsets.mylatex file='nestsuccess-SAS-data.tex' (notop nobot);
proc print data=nestsuccess(obs=10);
   var height Nnests Nsuccess phat elogit;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="nestsuccess-SAS-prelim" reset=index;
proc document name=plot1;
  replay \sgplot#1\sgplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="nestsuccess-SAS-elogit" reset=index;
proc document name=plot2;
  replay \sgplot#1\sgplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='nestsuccess-SAS-genmodest.tex' (notop nobot);
proc print data=genmodest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='nestsuccess-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="nestsuccess-SAS-genmodplot" reset=index;
proc document name=plot3;
  replay \sgplot#1\sgplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;






ods tagsets.mylatex file='nestsuccess-SAS-logisticest.tex' (notop nobot);
proc print data=logistic_estimates noobs label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="nestsuccess-SAS-logisticplot" reset=index;
proc document name=Logistic;
  replay \Logistic#1\EffectPlots#1\EffectPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


