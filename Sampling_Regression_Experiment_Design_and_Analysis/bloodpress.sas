/* Effect of age, weight, and stress index on blood pressure */
 
/* Blood pressure tends to increase with age, body mass, and potentially stress.
   To investigate the relationship of blood pressure to these variables, a
   sample of men in a large corporation was selected. For each subject,
   their age (years), body mass (kg), and a stress index (ranges from 0 to 100)
   was recorded along with their blood pressure.  */

/* 2015-07-20 CJS updated */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;

ods pdf file='bloodpress-SAS.pdf' style=styles.printer;

TITLE 'Effect of age, weight, and stress on blood pressure';
options nodate nonumber noovp;
 
*---partdatab;
DATA bloodpress;
   infile 'bloodpress.csv' dlm=',' dsd missover firstobs=2;
   INPUT Age BloodPressure StressIndex Weight;
run;
*---partdatae;

PROC PRINT DATA=bloodpress;
   TITLE2 'raw data';
run;

ods document name=scatter(write);
*---partscatterb;
proc sgscatter data=bloodpress;
   title2 'scatter plot matrix';
   matrix age stressindex weight bloodpressure;
run;
*---partscattere;
ods document close;
 
ods document name=reg(write);
*---partlmfitb;
PROC REG DATA=bloodpress plot=all;
   TITLE2 'REGRESSION ANALYSIS';
   MODEL BloodPressure = age weight StressIndex / clb partial ;
   output out=preddata  p=predicted r=residual
                        lclm=lcl_mean  uclm=ucl_mean
                        lcl =lcl_indiv ucl =ucl_indiv;
   ods output nobs          = lmnobs;
   ods output fitstatistics = lmfitstat;
   ods output anova         = lmanova;
   ods output parameterestimates=lmestimates;
run;
*---partlmfite;
ods document close;
 
ods pdf close;
   


/********************************************************************/
/* Generate latex code for inclusion into my notes    */

title ;
footnote ;

%include '../../MyLaTeXtagset.sas'; run;  /* See if my latex tagsets work */
          
/* the raw data */
ods tagsets.mylatex file="bloodpress-SAS-data.tex" (notop nobot) ;
proc print data=bloodpress noobs label split=' ';
run;
ods tagsets.mylatex close;

proc document name=reg;
   list / levels=all;
run;

ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-scatter' reset=index;
proc document name=scatter;
   replay \Sgscatter#1\SGScatter#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;

/* output from reg */
ods tagsets.mylatex file="bloodpress-SAS-lmnobs.tex" (notop nobot) ;
proc print data=lmnobs noobs label split=' ';
   var Model Dependent Label N;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="bloodpress-SAS-lmanova.tex" (notop nobot) ;
proc print data=lmanova noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="bloodpress-SAS-lmestimates.tex" (notop nobot) ;
proc print data=lmestimates noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="bloodpress-SAS-lmpredavg.tex" (notop nobot) ;
proc print data=preddata noobs label split=' ' ;
   var age stressindex weight bloodpressure predicted lcl_mean ucl_mean;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="bloodpress-SAS-lmpredindiv.tex" (notop nobot) ;
proc print data=preddata noobs label split=' ' ;
   var age stressindex weight bloodpressure predicted lcl_indiv ucl_indiv;
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-obsvspred' reset=index;
proc document name=reg;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\BloodPressure#1\DiagnosticPlots#1\ObservedByPredicted#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;

ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-residplot' reset=index;
proc document name=reg;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\BloodPressure#1\DiagnosticPlots#1\ResidualByPredicted#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;

ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-qqplot' reset=index;
proc document name=reg;
   replay  \Reg#1\MODEL1#1\ObswiseStats#1\BloodPressure#1\DiagnosticPlots#1\QQPlot#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;

ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-diagpanel' reset=index;
proc document name=reg;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\BloodPressure#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;

ods listing;
ods graphics on / imagefmt=png imagename='bloodpress-SAS-leverage' reset=index;
proc document name=reg;
   replay  	\Reg#1\MODEL1#1\PartialPlots#1\BloodPressure#1\PartialPlot#1 / dest=listing;
run; 
ods graphics off;
ods listing close;
run;


