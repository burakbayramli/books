/* French Creek was monitored for several months at two sites (Barclay Bride and Coombs).
   At the synpotic times, several water quality variables were measured, including
   Turbidity in NTU.
 
   We will compare the Turbidity between all the sites using an RCB analysis
  
   Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/

/* Analysis of French Creek Turbidity at all Sites using SAS */
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

title 'Comparing Turbidity at all sites measured same times - example of RCB analysis';

options nodate nonumber noovp orientation=landscape;
ods document name=work.output(write);  /* enable selection of output from SAS */
ods pdf file='TurbidityCompareRCB.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data turbidity;
   length SampleTime $10.;
   infile 'TurbidityCompare.csv' dlm=',' dsd missover firstobs=2;
   input SampleTime BB Coombs Grafton NewHwy WinchRd;;
run;
*---part001e;

proc print data=turbidity;
   title2 'raw data';
run;

 

/****************************** Now for a modelling approach ***********/
 
/* First stack the data */
*---part021b;
data turbidity3;
   set turbidity;
   length SiteName $10;
   sitename = "BB";      turbidity = BB;      output;
   sitename = 'Coombs';  turbidity = Coombs;  output;
   sitename = 'Grafton'; turbidity = Grafton; output;
   sitename = 'NewHwy';  turbidity = NewHwy;  output;
   sitename = 'WinchRd'; turbidity = WinchRd; output;
   keep SampleTime SiteName turbidity;
run;
data turbidity3; /* compute log(turbidity) */
   set turbidity3;
   logTurbidity = log(turbidity);
run;
*---part021e;
 
proc print data=turbidity3(obs=10);
   title2 'part of the transposed raw data';
run;

ods document name=mixed3(write);
*---part025b;
ods graphics on;
proc mixed data=turbidity3 plots=all;
   title2 'Modelling approach as an RCB using MIXED';
   class SampleTime SiteName;
   model logturbidity = SampleTime SiteName/ /*ddfm=KR */;
   lsmeans SiteName / diff pdiff adjust=tukey;  
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
*---part025e;
ods document close;

/* get the joined line plot using the pdmix800 macro and the output 
   from Mixed */
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

ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=turbidity;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-021.tex' (notop nobot);
proc print data=turbidity3(obs=10);
   title2 'part of the transposed raw data';
run;
ods tagsets.mycolorlatex close;
title;

ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-025.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-026.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-027.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='TurbidityCompareRCB-SAS-024' reset=index;
proc document name=mixed3;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='TurbidityCompareRCB-SAS-040.tex' (notop nobot);
%pdmix800(diffs3,lsmeans3,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;



run;
