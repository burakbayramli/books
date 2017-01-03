/* Seasonal adjustment using sine/cosine terms */
/* TEOM vs. Reference meter for PM 2.5 measurements.*/
/* 2014-08-11 CJS update for SAS 9.4 */
dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
footnote ' ';


options orientation=landscape;
ods pdf file='teom-SAS.pdf' style=styles.printer;

title 'TEOM vs Reference meter for PM 2.5';

/* 
The air that we breath often has many contaminants. One contaminant of interest
is {\bf Particulate Matter (PM)}.
Particulate matter is the general term used for a mixture of solid particles and liquid droplets in thej
air. It includes aerosols, smoke, fumes, dust, ash and pollen. 
The composition of particulate matter varies with place, season and weather conditions.
Particulate matter is characterized according to size - mainly because of 
the different health effects associated with particles of different diameters.
Fine particulate matter is particulate matter that is 2.5 microns in diameter and less.
[A human hair is approximately 30 times larger than these particles!
The smaller particles are so small that several thousand of them could fit on the period at the end of this sentence.
It is also knowas PM2.5 or respirable particles because it penetrates the respiratory system further than larger particles.

PM2.5 material is primarily formed from chemical reactions in the atmosphere and through fuel combustion (e.g., motor vehicles,
power generation, industrial facilities residential fire places, wood stoves and agricultural burning). Significant amounts of
PM2.5 are carried into Ontario from the U.S. During periods of widespread elevated levels of fine particulate matter, it is
estimated that more than 50 per cent of Ontario's PM2.5 comes from the U.S.

Adverse health effects from breathing air with a high PM 2.5 concentration include: premature death, increased respiratory
symptoms and disease, chronic bronchitis, and decreased lung function particularly for individuals with asthma.

Further information about fine particulates is available at many websites as
\url{http://www.health.state.ny.us/nysdoh/indoor/pmq_a.htm} and 
\url{http://www.airqualityontario.com/science/pollutants/particulates.cfm}, and
\url{http://www.epa.gov/pmdesignations/faq.htm}.

The PM2.5 concentrations in air can be measured in many ways. A well known method is a
is a filter based method whereby one 24 hour sample is collected every third day.
The sampler draws air through a pre-weighed filter for a specified period (usually 24 hours) at a known flowrate. The
filter is then removed and sent to a laboratory to determine the gain in filter mass due to particle collection. Ambient PM
concentration is calculated on the basis of the gain in filter mass, divided by the product of sampling period and sampling
flowrate. Additional analysis can also be performed on the filter to determine the chemical composition of the sample.

In recent years, a program of continuous sampling using automatic samplers has been introduced. An
instrument widely adopted for this use is the Tapered Element Oscillating Microbalance (TEOM). The TEOM operates under the
following principles. Ambient air is drawn in through a heated inlet. It is then drawn through a filtered cartridge on the end
of a hollow, tapered tube. The tube is clamped at one end and oscillates freely like a tuning fork. As particulate matter
gathers on the filter cartridge, the natural frequency of oscillation of the tube decreases. The mass accumulation of
particulate matter is then determined from the corresponding change in frequency. 

Because of the different ways in which these instruments work, a calibration experiment was performed
The hourly TEOM readings were accumated to a daily value and compared to those obtained
from an air filter method. 
*/

*---part010b;
data teom;
   infile 'teom.csv' firstobs=2 dlm=',' dsd missover;  /* skip the title of data */
   input date:yymmdd10. teom ref;
   logratio = log(teom/ref);
   year = 2003 + (date-'01jan2003'd)/365;
   cos = cos(2*3.14159*year/1);  /* period was 1 year */
   sin = sin(2*3.14159*year/1);
   format date yymmdd10.;
   format logratio cos sin 7.2;
run;
*---part010e;

proc print data=teom(obs=20);
   title2 'part of the raw data';
run;
 

ods document name=prelimplot(write);
*---partprelimplot;
proc sgplot data=teom;
   title2 'Preliminary data plot';
   scatter x=year y=logratio;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05 integer;
   yaxis label='log(TEOM/reference)';
run;
ods document close;


ods document name=sinfit(write);
*---partsinfitb;
/***** Fit a model to the year, sin and cosine terms */
ods graphics on;
proc reg data=teom plots=all;
   title2 'model with year, sin and cosine terms ';
   model logratio = year cos sin / dw dwprob p r clb;
   ods output OutputStatistics   =sinfitOutputStatistics;
   ods output ParameterEstimates =sinfitParameterEstimates;
   ods output DWstatistic        =sinfitDWstatistic;
run;
*---partsinfite;
ods document close;


proc print data=sinfitOutputStatistics(obs=10);
   title3 'part of the final fit';;
run;

data teom_fit;
   merge teom sinfitOutputStatistics;
run;

proc print data=teom_fit(obs=10);
run;

ods document name=sinfitplotfit;
proc sgplot data=teom_fit;
   title2 'Fitted model';
   scatter x=year y=logratio;
   series  x=year y=predictedvalue;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05 integer;
   yaxis label='log(TEOM/reference)';
run;
ods document close;


ods document name=sinfitsimple(write);
*---partsinfitsimpleb;
/* now to run the simpler model to estimate the intercept */
ods graphics on;
proc reg data=teom plots=all;
   title2 'model with sin and cosine terms - estimate the intercept ';
   model logratio = cos sin /dw dwprob;
   ods output ParameterEstimates =sinfitsimpleParameterEstimates;
   ods output DWstatistic        =sinfitsimpleDWstatistic;
run;
*---partsinfitsimplee;
ods document close;

ods pdf close;



/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=sinfit;
   list /levels=all;
run;

ods tagsets.mylatex file='TEOM-SAS-010.tex' (notop nobot);
proc print data=TEOM(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='TEOM-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods listing;
ods graphics on / imagefmt=png imagename='TEOM-SAS-sinfitplotfit' reset=index;
proc document name=sinfitplotfit;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods listing;
ods graphics on / imagefmt=png imagename='TEOM-SAS-sinfitdiagplot' reset=index;
proc document name=sinfit;
   replay  \Reg#1\MODEL1#1\ObswiseStats#1\logratio#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='TEOM-SAS-sinfitparameterestimates.tex' (notop nobot);
proc print data=sinfitParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='TEOM-SAS-sinfitdwstatistics.tex' (notop nobot);
proc print data=sinfitdwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='TEOM-SAS-sinfitsimpleparameterestimates.tex' (notop nobot);
proc print data=sinfitsimpleParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;









 

   



   



