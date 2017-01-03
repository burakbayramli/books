/* Systolic Blood Pressure under a variety of conditions.
   Dataset from Claire Protheroe, BPK.
   
The file includes data for systolic blood pressure measures in 
three different tests. The fifteen subjects took part in all three tests 
so we have a paired data set. However, for each individual, some tests were 
longer than others, so there are different numbers of data points for 
each individual for each of the three tests.
*/

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
run;

title 'Systolic Blood Pressure under 3 Stockings and different phases';

options nodate noovp orientation=landscape; 
ods pdf file='SystolicBloodPressure-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


/* get the raw data file */
*---part001b;
proc import out=bp file='SystolicBloodPressure.csv' dbms=csv replace;
run;
*---part001e;

proc print data=bp(obs=10);
run;



proc tabulate data=bp missing;
   title2 'summary of the raw data';
   class subject Stocking phase;
   var SysBP;
   table subject, Stocking*phase*SysBP*(n*f=5.0 mean*f=6.2 std*f=6.2);
run;


/* average over the replicated measurements. Drop the -60 mmHg because of poor sample size */
*---part005b;
data bp2;
   set bp;
   if index(phase,'-60')>0 then delete;  /* discard this phase */
run;
proc sort data=bp2;
   by subject Stocking phase;
run;
proc means data=bp2 noprint;
   by subject Stocking phase;
   var SysBP;
   output out=mean_bp mean=mean_SysBP;
run;
*---part005e;
 
/* create the combinations of Stocking (stockings) and phase for the profile plots */
data mean_bp; /* drop the -60 mmHg */
   set mean_bp;
   length trt $20.;
   if index(lowcase(Stocking),'placebo')>0 and index(Stocking,'1')>1 then trt = 'P1.';
   if index(lowcase(Stocking),'placebo')>0 and index(Stocking,'2')>1 then trt = 'P2.';
   if index(lowcase(Stocking),'exp')>0                                then trt = 'EX.';
   trt = compress(trt || phase);
run;

/* the profile plot of the responses */
proc sort data=mean_bp;
   by trt subject;
run;
ods document name=Profile(write);
*---part010b;
proc sgplot data=mean_bp;
   title2 'Profile and dot plot of mean responses';
   scatter x=trt y=mean_SysBP / markerattrs=(symbol=circlefilled);
   series  x=trt y=mean_SysBP / group=subject lineattrs=(color=black pattern=1);
run;
*---part010e;
ods document close;


/* make a plot of what is happening */
proc sort data=mean_bp;
   by trt;
run;
proc gplot data=mean_bp;
   title2 'Blood pressure profile plots';
   axis1 label=('Time') offset=(1 cm, 1 cm);
   plot mean_SysBP*trt=subject / href=21 41 51 61 haxis=axis1;
   symbol1 v=none i=line r=20;
run;

ods document name=mixed(write);
*---part015b;
ods graphics on;
proc mixed data=mean_bp plots=all;
   title2 'Split-plot with main plots in blocks';
   class subject Stocking phase;
   model mean_SysBP = Stocking | phase / ddfm=kr;
   random subject subject*Stocking;
   lsmeans phase / diff cl adjust=tukey;
   ods output lsmeans=lsmeans;
   ods output tests3 =tests;
   ods output diffs  =lsmeansdiffs;
run;
ods graphics off;
*---part015e;
ods document close;


/* get the joined line plots */
ods document name=JoinedLines(write);
*---part020b;
%include '../../pdmix800.sas'; run;
data tempdiffs;
   set lsmeansdiffs;
   if index(lowcase(effect),'phase')>0 and index(lowcase(effect),'Stocking')=0;
run;
data tempmeans;
   set lsmeans;
   if index(lowcase(effect),'phase')>0 and index(lowcase(effect),'Stocking')=0;
run;
%pdmix800(tempdiffs, tempMeans, alpha=0.05, sort=yes);
*---part020e;
ods document close;


/* analysis on the individual measurement using a sub-sampling model */
ods graphics off;
proc mixed data=bp2;
   title2 'Analysis using a sub-sampling model';
   class Stocking phase subject;
   model SysBP = Stocking | phase / ddfm=kr;
   random subject subject*Stocking  subject*phase*Stocking;
   lsmeans phase / diff cl adjust=tukey;
   ods output lsmeans = lsmeans2;
   ods output diffs   = diffs2;
run;
ods graphics on;
%pdmix800(diffs2, lsMeans2, alpha=0.05, sort=yes);


ods pdf close;








/* Create LaTeX files for inclusion by my notes */
%include "../../MyLaTeXtagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='SystolicBloodPressure-SAS-001.tex' (notop nobot);
proc print data=bp(obs=10);
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="SystolicBloodPressure-SAS-010" reset=index;
proc document name=Profile;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;

ods tagsets.mycolorlatex file='SystolicBloodPressure-SAS-015.tex' (notop nobot);
proc print data=Tests noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='SystolicBloodPressure-SAS-016.tex' (notop nobot);
proc print data=lsmeansDiffs noobs split=' ' label;
   var effect phase _phase estimate stderr adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='SystolicBloodPressure-SAS-020.tex' (notop nobot);
proc document name=JoinedLines;
  obtitle \Print#1\ByGroup1#1\Print#1 ;
  obfootn \Print#1\ByGroup1#1\Print#1 ;
  replay \Print#1\ByGroup1#1\Print#1 / levels=all;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="SystolicBloodPressure-SAS-030" reset=index;
proc document name=mixed;
  replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / levels=all;
run;
ods graphics off;
