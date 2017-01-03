/* Factors infuencing satelitte males at female crabs
 
These concepts will be illustrated using a dataset on nesting horseshoe crabs that is analyzed in
Agresti's book

The design of the study is given in Brockmann H.J. (1996). Satellite male groups in horseshoe crabs,
Limulus polyphemus. Ethology, 102, 1-21. 

Each female horseshoe crab had a male resident in her nest. The study investigated other factors
affecting whether the female had any other males, called satellites residing nearby. These other factors
includes:
    - crab color where 2=light medium, 3=medium, 4=dark medium, 5=dark.
    - spine condition where 1=both good, 2=one worn or broken, or 3=both worn or broken.
    - weight
    - carapace width
The number of satellites was measured; for this example we will convert the number of satellite males
into a presence (number at least 1) or absence (no satellites). */

/*  Change log
    2015-07-13 CJS First edition */

/* Lines starting with *---part001b; or *---part001e; bracket the source 
   line for inclusion by LaTex and usually are not coded. */

 
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;


title 'What factor influence presence/absence of satellite males?';
ods pdf file='crabsatellites-logit-SAS.pdf' style=styles.printer;
 
*---part001b;
data crabs;
    infile 'crabsatellites.csv' dlm=',' dsd missover firstobs=2;
    length males $8 weightclass $10;
    input color spine width NMales Weight Males $ WeightClass $;
run;
*---part001e;

proc print data=crabs(obs=10);
   title2 'part of the raw data';
run;


ods document name=scatter(write);
/* Preliminary scatter plot */
proc sgscatter data=crabs;
   title2 'Preliminary Scatter plot';
   matrix color spine width weight NMales;
run;
ods document close;


ods document name=weightplot(write);
/* plot of weight vs width */
proc sgplot data=crabs;
   title2 'Weight vs Width';
   scatter x=Width y=Weight;
run;
ods document close;


/* remove the outliers */
data crabs outliers;
   set crabs;
   outlier=0;
   if weight < 1500 and width > 25 then outlier = 1; 
   if width < 21.5 then outlier=1;
   if width > 33   then outlier=1;
   if outlier=1 then output outliers;
                else output crabs;
run;
 
proc print data=outliers;
   title2 'outliers removed from data';
run;

*---partprelimcompb;
/* compute the proportion of female crabs with satellite males in each weight class */
data crabs2;
   set crabs;
   pmale=0;
   if males = 'yes' then pmale=1;
run;
proc sort data=crabs2; by color weightclass; run;
proc means data=crabs2 noprint; 
   by color weightclass;
   var pmale;
   output out=pmale n=ncrabs mean=pmale;
run;
*---partprelimcompe;

proc print data=pmale;
   title2 'Empirical proportion of satellite males';
   var color weightclass ncrabs pmale;
   format pmale 7.3;
run;


*---partprofileb;
/* Create profile plots of the summarized */
/* in order to offset the standard error bars, we need to create a special code
   for the weightclass variable with special formats for plotting */
data plotdata;
   set pmale;
   pmale_lcl = max(0,pmale - 1.96*sqrt(pmale*(1-pmale)/ncrabs));
   pmale_ucl = min(1,pmale + 1.96*sqrt(pmale*(1-pmale)/ncrabs));
   if weightclass = '0000-2000' then weightclass2 = 1;
   if weightclass = '2000-2500' then weightclass2 = 2;
   if weightclass = '2500-3000' then weightclass2 = 3;
   if weightclass = '3000+'     then weightclass2 = 4;

   if color = 2 then weightclass2 = weightclass2 - .1;
   if color = 3 then weightclass2 = weightclass2 - .05;
   if color = 4 then weightclass2 = weightclass2 + .0;
   if color = 5 then weightclass2 = weightclass2 + .05;
run;
*proc print data=plotdata;
*run;
proc format; 
   value weightclassfmt  1='0000-2000' 2='2000-2500' 3='2500-3000'  4='3000+';
run;
 
ods document name=profile1(write);
proc sgplot data=plotdata;
   title2 'Preliminary profile plot';
   series  x=weightclass2 y=pmale / group=color;
   highlow x=weightclass2 low=pmale_lcl high=pmale_ucl / group=color;
   format weightclass2 weightclassfmt.;
   xaxis label='Weight Class' values=(1,2,3,4)  integer offsetmin=.07 offsetmax=0.07;
   yaxis label='p(satellite male) with 95% ci';
run;
ods document close;
*---partprofilee;




*---partgenmodfitb;
ods document name=genmod1(write);
/********* using GENMOD - full model ********/ 
proc genmod data=crabs plots=all descending ;
   class color;
   model males = weight | color / dist=binomial link=logit  type3;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
run;
*---partgenmodfite;
ods document close;




ods document name=genmod2(write);
*---partgenmodredfitb;
/********* using GENMOD - reduced model ********/ 
proc genmod data=crabs plots=all ;
   class color;
   model males = color weight / dist=binomial link=logit  type3;
   output out=genmodredpred  xbeta=xbeta pred=pred_pmale lower=lcl_pmale upper=ucl_pmale;
   lsmeans color / diff cl adjust=tukey ilink oddsratio;
   estimate 'color 5 vs avg rest' color -1 -1 -1 3 / divisor=3;
   ods output parameterestimates=genmodredest;
   ods output type3             =genmodredtest;
   ods output lsmeans           =genmodredlsmeans;
   ods output diffs             =genmodredlsmeansdiffs;
   ods output estimates         =genmodredestimates;
run;
*---partgenmodredfite;
ods document close;
 
 
ods document name=finalprofile(write);
proc sort data=genmodredpred; by color weight; run;
proc sgplot data=genmodredpred;
   title2 'Predicted values from the reduced model';
   series  x=weight y=pred_pmale / group=color;
   *band    x=weight lower=lcl_pmale upper=ucl_pmale / group=color;
   xaxis label='Weight';
   yaxis label='Predicted P(satelitte alive)';
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

ods tagsets.mylatex file='crabsatellites-logit-SAS-data.tex' (notop nobot);
proc print data=crabs(obs=10);
run;
ods tagsets.mylatex close;

/* the scatterplot matrix */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='crabsatellites-logit-SAS-pairsplot' reset=index;
proc document name=scatter;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* the weight vs width plot matrix */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='crabsatellites-logit-SAS-weightplot' reset=index;
proc document name=weightplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



/* proportion of males by color and weight class */
ods tagsets.mylatex file='crabsatellites-logit-SAS-prelimcomp.tex' (notop nobot);
proc print data=pmale;
   var color weightclass ncrabs pmale;
   format pmale 7.3;
run;
ods tagsets.mylatex close;

/* preliminary profile plot */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='crabsatellites-logit-SAS-prelimplot' reset=index;
proc document name=profile1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



/* genmod output from the full model */
ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;



/* genmod output from the reduced model */
ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodredest.tex' (notop nobot);
proc print data=genmodredest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodredtest.tex' (notop nobot);
proc print data=genmodredtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodredestimates.tex' (notop nobot);
proc print data=genmodredestimates noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodredlsmeandiff1.tex' (notop nobot);
proc print data=genmodredlsmeansdiffs noobs label split=" ";
   var effect color _color estimate stderr;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='crabsatellites-logit-SAS-genmodredlsmeandiff2.tex' (notop nobot);
proc print data=genmodredlsmeansdiffs noobs label split=" ";
   var effect color _color oddsratio lowerOR upperOR;
run;
ods tagsets.mylatex close;


/* final profile plot */
ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='crabsatellites-logit-SAS-finalprofileplot' reset=index;
proc document name=finalprofile;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

