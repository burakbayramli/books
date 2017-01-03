/* Estimate the probability of failure of o-ring on space shuttle as function of launch temperature */
/* Logistic regression */
/* 2015-07-05 CJS Update with newer versions of sas */


/* Actual data after the Challenger disaster */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;

title 'Does temperature affect failure rate of o-ring?';
ods pdf file='spaceshuttleoring-SAS.pdf' style=styles.printer; 

*---part001b;
data oring;
   infile 'spaceshuttleoring.csv' dlm=',' dsd missover firstobs=2;
   input temperature outcome $;
   if temperature=31 then outcome = ' ';  /* set to missing for prediction purposes */
   phat = 0;
   if outcome = 'f' then phat=1;  /* create 0/1 variable for plotting */
run;
*---part001e;

proc print data=oring;
   title2 'raw data';
run;

*---partpreddatab; 
/* make a dataset suitable for predictions */ 
data oring2;
   do temperature =30 to 90 by 0.2;
      outcome = ' ';
      output;
   end;
run;
 
data oring;
   set oring oring2;
run;
*---partpreddatae;

proc sort data=oring; by temperature; run;
 
ods document name=genmod1(write);
*---part050b;
/********* using GENMOD ********/
proc genmod data=oring;
   title2 'fit a generalized linear model';
   model outcome = temperature / dist=binomial link=logit type3;
   output out=genmod_fit p=pred l=lower u=upper resdev=resdev;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
run;
*---part050e;
ods document close;

proc print data=genmod_fit;
   title2 'predictions from the fit';
   where 31.5 < temperature < 32.5;
run;


ods document name=plot3(write);
/* Produce a plot of the fitted curves and confidence envelope */
proc sgplot data=genmod_fit;
   title2 'Predicted plot from GENMOD';
   band    x=temperature lower=lower upper=upper / transparency=0.5;
   scatter x=temperature y=phat  / markerattrs=(symbol=circlefilled color=black);
   series  x=temperature y=pred  / lineattrs  =(color=black);
run;
ods document close;

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

ods tagsets.mylatex file='spaceshuttleoring-SAS-data.tex' (notop nobot);
proc print data=oring;
    where outcome ^= ' ' and temperature < 70;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='spaceshuttleoring-SAS-genmodest.tex' (notop nobot);
proc print data=genmodest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='spaceshuttleoring-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='spaceshuttleoring-SAS-genmodpred.tex' (notop nobot);
proc print data=genmod_fit;
   title2 'predictions from the fit';
   where 31.5 < temperature < 32.5;
run;
ods tagsets.mylatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename="spaceshuttleoring-SAS-genmodplot" reset=index;
proc document name=plot3;
  replay \sgplot#1\sgplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


