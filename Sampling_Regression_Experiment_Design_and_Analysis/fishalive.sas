/* Impact of different levels of Selenium on the deformity proportions of fish
 
  A fish is a popular pet for young children – yet the survival rate of many of these fish is likely poor.
  What factors seem to influence the survival probabilities of pet fish?
  A large pet store conducted a customer follow-up survey of purchasers of pet fish. A number of
  customers were called and asked about the hardness of the water used for the fish 
  (soft, medium, or hard), where the fish was kept which was then classified into
  cool or hot locations within the living
  dwelling, if they had previous experience with pet fish (yes or no), and if the pet fish was alive six
  months after purchase (yes or no).  */


/*  Change log
    2015-07-08 CJS First edition */

/* Lines starting with *---part001b; or *---part001e; bracket the source 
   line for inclusion by LaTex and usually are not coded. */

 
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;


title 'What factor influence fish survival?';
ods pdf file='fishalive-SAS.pdf' style=styles.printer;
 
*---part001b;
data fish;
    infile 'fishalive.csv' dlm=',' dsd missover firstobs=2;
    length softness temperature prevfish $8;
    input softness $ temperature$ prevfish $ trials alive;
	dead   = trials - alive;
	palive = alive /trials;
	logitalive = log( alive/dead);
run;
*---part001e;

proc print data=fish;
   title2 'raw data';
run;

*---partprofileb;
/* Create profile plots of the original data */
/* in order to offset the standard error bars, we need to create a special code
   for the softness variable with special formats for plotting */
data plotdata;
   set fish;
   palive_lcl = palive - 1.96*sqrt(palive*(1-palive)/trials);
   palive_ucl = palive + 1.96*sqrt(palive*(1-palive)/trials);
   logitalive_lcl  = logitalive - 1.96*sqrt(1/(palive*(1-palive)*trials));
   logitalive_ucl  = logitalive + 1.96*sqrt(1/(palive*(1-palive)*trials));
   tempprev = substr(temperature,1,1) || "." || substr(prevfish,1,1);
   if softness = 'h' then softness2 = 1;
   if softness = 'm' then softness2 = 2;
   if softness = 's' then softness2 = 3;
   if tempprev = 'c.n' then softness2 = softness2 - .1;
   if tempprev = 'h.n' then softness2 = softness2 - .05;
   if tempprev = 'c.y' then softness2 = softness2 + .0;
   if tempprev = 'h.y' then softness2 = softness2 + .05;
run;
proc print data=plotdata;
run;
proc format; 
   value softfmt  1='h' 2='m' 3='s';
run;
 
ods document name=profile1(write);
proc sgplot data=plotdata;
   title2 'probability scale';
   series  x=softness2 y=palive / group=tempprev;
   highlow x=softness2 low=palive_lcl high=palive_ucl / group=tempprev;
   format softness2 softfmt.;
   xaxis label='Softness' values=(1,2,3)  integer offsetmin=.07 offsetmax=0.07;
   yaxis label='p(alive) with 95% ci';
run;
ods document close;

ods document name=profile2(write);
proc sgplot data=plotdata;
   title2 'logit scale';
   series  x=softness2 y=logitalive / group=tempprev;
   highlow x=softness2 low=logitalive_lcl high=logitalive_ucl / group=tempprev;
   format softness2 softfmt.;
   xaxis label='Softness' values=(1,2,3)  integer offsetmin=.07 offsetmax=0.07;
   yaxis label='logit(alive) with 95% ci';
run;
ods document close;
*---partprofilee;


*---partgenmodfitb;
/* Fit the logistic models */
/* we need to stack the data */
data fishlong;
   set fish;
   length status $5;
   status = 'alive'; count = alive; output;
   status = 'dead';  count = dead;  output;
   keep softness temperature prevfish status count;
run;


ods document name=genmod1(write);
/********* using GENMOD - full model ********/ 
proc genmod data=fishlong plots=all ;
   class softness temperature prevfish;
   model status = softness | temperature | prevfish / dist=binomial link=logit  type3;
   freq count;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
run;
*---partgenmodfite;
ods document close;


ods document name=genmod2(write);
*---partgenmodredfitb;
/********* using GENMOD - reduced model ********/ 
proc genmod data=fishlong plots=all ;
   class softness temperature prevfish;
   model status = softness | prevfish  temperature / dist=binomial link=logit  type3;
   freq count;
   output out=genmodredpred  xbeta=xbeta pred=pred_palive lower=lcl_palive upper=ucl_palive;
   lsmeans softness*prevfish / diff cl adjust=tukey ilink oddsratio;
   ods output parameterestimates=genmodredest;
   ods output type3             =genmodredtest;
   ods output lsmeans           =genmodredlsmeans;
   ods output diffs             =genmodredlsmeansdiffs;
run;
*---partgenmodredfite;
ods document close;



/* Create plot of predicted vs actual logit values */
proc sort data=fish;          by softness temperature prevfish; run;
proc sort data=genmodredpred; by softness temperature prevfish; run;
data plotdata;
   merge fish genmodredpred; 
   by softness temperature prevfish;
run;

ods document name=obsvspred(write);
proc sgplot data=plotdata;
   title2 'actual vs predicted logit';
   scatter y=logitalive x=xbeta;
   lineparm slope=1 x=1 y=1;
   xaxis label='Predicted logit(alive) from reduced model';
   yaxis label='Observed logit(alive) from data';
run;
ods document close;



/* Profile plot of the fitted model */
data plotdata;
   set genmodredpred;
   if status = 'alive';
   logitalive      = xbeta;
   logitalive_lcl  = log(lcl_palive/(1-lcl_palive));
   logitalive_ucl  = log(ucl_palive/(1-ucl_palive));
   tempprev = substr(temperature,1,1) || "." || substr(prevfish,1,1);
   if softness = 'h' then softness2 = 1;
   if softness = 'm' then softness2 = 2;
   if softness = 's' then softness2 = 3;
   if tempprev = 'c.n' then softness2 = softness2 - .1;
   if tempprev = 'h.n' then softness2 = softness2 - .05;
   if tempprev = 'c.y' then softness2 = softness2 + .0;
   if tempprev = 'h.y' then softness2 = softness2 + .05;
run;
proc print data=plotdata;
run;
proc format; 
   value softfmt  1='h' 2='m' 3='s';
run;
 
ods document name=finalprofile(write);
proc sgplot data=plotdata;
   title2 'logit scale - fitted reduced model';
   series  x=softness2 y=logitalive / group=tempprev;
   highlow x=softness2 low=logitalive_lcl high=logitalive_ucl / group=tempprev;
   format softness2 softfmt.;
   xaxis label='Softness' values=(1,2,3)  integer offsetmin=.07 offsetmax=0.07;
   yaxis label='logit(alive) with 95% ci';
run;
ods document close;




ods pdf close;

/* now create the LaTeX files for inclusion in the course notes */
%include "../../MyLatexTagset.sas"; run;
ods listing close;
title;
footnote;
ods listing;

proc document name=genmod1;
 list /levels=all;
run;

ods tagsets.mylatex file='fishalive-SAS-data.tex' (notop nobot);
proc print data=fish;
run;
ods tagsets.mylatex close;

/* the two profile plots */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishalive-SAS-profile1' reset=index;
proc document name=profile1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishalive-SAS-profile2' reset=index;
proc document name=profile2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



/* genmod output from the full model */
ods tagsets.mylatex file='fishalive-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;


/* genmod output from the reduced model */
ods tagsets.mylatex file='fishalive-SAS-genmodredest.tex' (notop nobot);
proc print data=genmodredest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fishalive-SAS-genmodredtest.tex' (notop nobot);
proc print data=genmodredtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fishalive-SAS-genmodredlsmeandiff1.tex' (notop nobot);
proc print data=genmodredlsmeansdiffs noobs label split=" ";
   var effect softness prevfish _softness _prevfish estimate stderr;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='fishalive-SAS-genmodredlsmeandiff2.tex' (notop nobot);
proc print data=genmodredlsmeansdiffs noobs label split=" ";
   var effect softness prevfish _softness _prevfish oddsratio lowerOR upperOR;
run;
ods tagsets.mylatex close;

/* observed vs predicted plots */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishalive-SAS-obsvspred' reset=index;
proc document name=obsvspred;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* final profile plot */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishalive-SAS-finalprofileplot' reset=index;
proc document name=finalprofile;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

