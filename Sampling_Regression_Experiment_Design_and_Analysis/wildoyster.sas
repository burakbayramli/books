/* Example of a two stage survey design */
/* 2015-07-04 CJS Updated for latest version of SAS */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;




/* Oyster survey by Klahoose First Nations at Lloyd Point in 1994  */

/*A First Nation wished to develop a wild oyster fishery. 
  As first stage in the development of the fishery, 
  a survey was needed to establish
  the current stock in a number of oyster beds.

  This example looks at the estimate of oyster numbers 
  from a survey conducted in 1994.

  The survey was conducted by a line through the oyster bed --
  the total length was 105 m. Several random Transect locations were located
  along the line. At each randomly chosen Transect,
  the width of the bed was measured and about 3 random location along
  the perpendicular Transect at that point were taken. A 1 m^2 quadrat
  was applied, and the number of oysters of various sizes was counted
  in the quadrat.	

  The Transect are a cluster (randomly chosen). Within each cluster
  three points were randomly selected. This is an example of a two-stage
  design. */

title 'Klahoose Oyster Survey - Example of two stage sample design';
ods pdf file='wildoyster-SAS.pdf' style=styles.printer;

*---part001b;
data oyster;
   infile 'wildoyster.csv' dlm=',' dsd missover firstobs=2;
   input loc $ transect width quad small xsamll small med large total weight;
   sampweight = 105/10 * width/3; /* sampling weight = product of sampling fractions */
run;
*---part001e;

proc print data=oyster;
   title2 'raw data';
run;


*---part002b;
/* estimate the total biomass on the oyster bed */
/* Note that SurveyMeans only use a first stage variance in its
   computation of the standard error. As the first stage sampling
   fraction is usually quite small, this will tend to give only
   slight underestimates of the true standard error of the estimate */

proc surveymeans data=oyster 
   total=105     /* length of first reference line */
   mean clmean
   sum clsum  ;       /* interested in total biomass estimate */
   cluster transect;  /* identify the perpindicular transects */ 
   var weight;
   weight sampweight;
   ods output statistics=oysterresults;
run;
*---part002e;
 

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


ods tagsets.mylatex file="wildoyster-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=oyster(obs=10);
run;
ods tagset.mylatex close;


ods tagsets.mylatex file="wildoyster-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=oysterresults noobs label split=" ";
   var varname  mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean  stderr upperCLmean 10.1 Sum LowerCLSum stddev UpperCLsum 10.3;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;
