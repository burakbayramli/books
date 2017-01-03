/* Regression through the origin; ANCOVA */

/* Condition Factors for Rainbow Trout  */

/*
  A sample of fish was captured by MOE using two different nets of standard 
  mesh sizes. We wish to compute several condition factors and see if males/
  females or maturity classes differ in their condition factor.

  Can W fishury be predicted from food W diets?

  Food W diets (e.g. number of fish, species of fish, etc) were 
  recorded for a sample of people. Based on estimates of fishury in the 
  food, the fishury in the diet was estimated.  
  A W sample was also taken from these people 
  and the W fishury level was also 
  measured.
*/

/* 2015-07-23 CJS revised for latest version of SAS */

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
dm 'output' clear;
dm 'log'    clear
proc datasets kill; run;
title; footnote; run;
ods graphics on; run;


title 'Estimating rainbow trout condition factors';
options nodate noovp orientation=landscape;
ods pdf file='rainbow-condition-SAS.pdf' style=styles.printer;
 
*---partdatab;
data fish;
   infile 'rainbow-condition.csv' dlm=',' dsd missover firstobs=2 ;
   input net_type $ fish length weight species $ sex $ maturity $;
   lenmod = (length/100)**3;
   label lenmod='(length/100)**3';
   condition_factor = weight / lenmod;
run;
*---partdatae;

proc print data=fish(obs=10);
   title2 'part of the raw data';
run;


ods document name=initplot(write);
*---part020b;
proc sgplot data=fish;
   title2 'Preliminary data plot';
   scatter y=weight x=lenmod /  markerattrs=(symbol=circlefilled);
   yaxis label='Weight'       offsetmin=.05 offsetmax=.05;
   xaxis label='(Length/100)**3'  offsetmin=.05 offsetmax=.05;
run;
*---part020e;
ods document close;


/* Create histogram of the individual condition factors */
ods document name=histogram(write);
*---part022b;
proc sgplot data=fish;
   title2 'histogram of individual condition factors';
   histogram condition_factor;
run;
*---part022e;
ods document close;


/* fit the linear model with no intercept*/
ods document name=regfit(write);
*---partregfitb;
proc reg data=fish plots=all;
   title2 'fit the model with NO intercept';
   model weight = lenmod / noint all;
   ods output OutputStatistics  =Predictions;
   ods output ParameterEstimates=Estimates;
run;
*---partregfite;
ods document close;
 
*---part040b;
data predictions;  /* merge the original data set with the predictions */
   merge fish predictions; 
run;
*---part040e;

proc print data=predictions(obs=10);
   title2 'Predicted values and confidence intervals';
run;


ods document name=ciplot(write);
*---part050b;
proc sgplot data=Predictions;
   title2 'Fitted line and confidence curves for mean and individual values';
   band    x=lenmod lower=lowerCL     upper=upperCL;
   band    x=lenmod lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=lenmod;
   scatter y=weight x=lenmod /  markerattrs=(symbol=circlefilled);
   yaxis label='weight'       offsetmin=.05 offsetmax=.05;
   xaxis label='lenmod'  offsetmin=.05 offsetmax=.05;
run;
*---part050e;
ods document close;




/* Now for ANOVA to test for equality among maturity classes */
ods document name=matfit(write);
*---partmatfitb;
proc glm data=fish plot=all;
   title2 'test for equal K among maturity classes';
   class maturity;
   model weight = lenmod lenmod*maturity / noint solution ;
   ods output ModelAnova         = matfitanova;
   ods output parameterestimates = matfitslopes;
   ods output estimates          = matfitestimates;
   estimate 'IM K'  lenmod 1 lenmod*maturity 1 0 / e;
   estimate 'M K'   lenmod 1 lenmod*maturity 0 1 / e;
run;
*---partmatfite;
ods document close;


/* Now for ANOVA to test for equality among sex classes */
ods document name=sexfit(write);
*---partsexfitb;
proc glm data=fish plot=all;
   title2 'test for equal K among sex classes';
   class sex;
   model weight = lenmod lenmod*sex / noint solution ;
   ods output ModelAnova         = sexfitanova;
   ods output parameterestimates = sexfitslopes;
   ods output estimates          = sexfitestimates;
   ods output OverallAnova       = sexfitoverallanova;
   estimate 'F K'  lenmod 1 lenmod*sex 1 0 0 / e;
   estimate 'M K'   lenmod 1 lenmod*sex 0 1 0 / e;
   estimate 'U K'   lenmod 1 lenmod*sex 0 0 1 / e;
run;
*---partsexfite;
ods document close;


*---partsexlackoffitb;
/* check for lack of fit using pure error */
/* We need to fit a more complex model with a factor with a separate
   level for each combination of sex and length */
/* Then we use the general linear test to compute the lack of fit */
data fish;
   set fish;
   length sexL $16;
   sexL = substr(sex,1,1) || '.' || put(length,7.0);
run;
proc print data=fish(obs=10);
   title2 'create separate level for each combination of sex and L';
run;
proc glm data=fish;
   title2 'Fit model to extract pure error SS';
   class sexL;
   model weight = sexL /  solution ;
   ods output OverallAnova         = purefitoverallanova;
run;
data lackoffit;;
   set sexfitoverallanova  purefitoverallanova;
   if source = 'Error';
   drop ms fvalue probf dependent source;
run;
proc print data=lackoffit; 
   title2 'sums of squares error used in lack of fit test';
run;
data lackoffit_res;
	obs=1; set lackoffit point=obs;
	df_sex = df; ss_sex =  ss;
	obs = 2; set lackoffit point=obs;
	df_sexL = df; ss_sexL = ss;
	/* now compute the F ratio and p-value */
	F = (ss_sex - ss_sexL)/(df_sex-df_sexL)/ (ss_sexL /df_sexL);
    ProbF = 1- cdf("F", F, df_sex-df_sexL, df_sexL);
	output;
	drop df ss;
	stop;
run;
proc print data=lackoffit_res;
   title2 'Results of lack of fit test ';
run; 
*---partsexlackoffite;
  




 



ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regfit;
   list /levels=all;
run;

/* regular fit */
ods tagsets.mylatex file='rainbow-condition-SAS-data.tex' (notop nobot);
proc print data=fish(obs=10 drop=sexL) label split="_";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-prelim' reset=index;
proc document name=initplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-hist' reset=index;
proc document name=histogram;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='rainbow-condition-SAS-regestimates.tex' (notop nobot);
proc print data=estimates noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='rainbow-condition-SAS-040.tex' (notop nobot);
proc print data=Predictions(obs=10) label split=" ";
   var lenmod weight PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-regfit' reset=index;
proc document name=ciplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-050b' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\weight#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-regdiagplot' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\weight#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;




/* Testing for effect of maturity level */
ods tagsets.mylatex file='rainbow-condition-SAS-matfitanova.tex' (notop nobot);
proc print data=matfitanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='rainbow-condition-SAS-matfitestimates.tex' (notop nobot);
proc print data=matfitestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods listing; /* fitted lines */
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-matfitplot' reset=index;
proc document name=matfit;
   replay \GLM#1\ANOVA#1\weight#1\ANCOVAPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;;


/* testing effect of sex */

/* Testing for effect of maturity level */
ods tagsets.mylatex file='rainbow-condition-SAS-sexfitanova.tex' (notop nobot);
proc print data=sexfitanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='rainbow-condition-SAS-sexfitestimates.tex' (notop nobot);
proc print data=sexfitestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods listing; /* fitted lines */
ods graphics on / imagefmt=png imagename='rainbow-condition-SAS-sexfitplot' reset=index;
proc document name=sexfit;
   replay \GLM#1\ANOVA#1\weight#1\ANCOVAPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;;


/* Lack of fit test for sex */
ods tagsets.mylatex file='rainbow-condition-SAS-sexlackoffit.tex' (notop nobot);
proc print data=lackoffit_res;
   title2 'Results of lack of fit test ';
run;
ods tagsets.mylatex close;
 
  
