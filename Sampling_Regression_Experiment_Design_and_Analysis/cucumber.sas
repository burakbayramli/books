/* Example of a cluster sample */
/* 2015-07-04 CJS Update for latest version of SAS */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;


/* Sea cucumbers are considered a delicacy among some, and the fishery
   is of growing importance.

   In order to set harvest quotas and in order to monitor the stock,
   it is important that number of sea cucumbers in a certain harvest area
   be estimated each year.

   The following is an example taken from Griffith Passage in  BC 1994.

   To do this, the managers lay out a number of transects
   across the cucumber harvest area.
   Divers then swim along the transect, and while carrying a 4 m wide
   pole, count the number of cucumbers within the width of the pole
   during the swim.

   The number of possible transects is so large that the correction
   for finite population sampling can be ignored. 

   The total bed area and the length of the shore are:

   Bed Area        3,769,280    m^2
   Shorelength      51426       m

   The raw data is already summarized to the transect level.  

                Total
Transect        Sea
Area            Cucumbers
 260            124
 220             67
 200              6
 180             62
 120             35
 200              3
 200              1
 120             49
 140             28
 400              1
 120             89
 120            116
 140             76
 800             10
1460             50
1000            122
 140             34
 180            109
  80             48  */



title 'Estimating cucumber density - example of cluster analysis with data presummarized to cluster level';
ods pdf file='cucumber-SAS.pdf' style=styles.printer;

*---part001b; 
data cucumber;
   infile 'cucumber.csv' dlm=',' dsd missover firstobs=2;
   input area cucumbers;
   transect = _n_;    /* number the transects */
run;
*---part001e;

*---part003b;
/* First compute the sampling weight and add to the dataset */
/* The sampling weight is simply the total pop size / # sampling units in an SRS */
/* In this example, transects were an SRS from all possible transects */

proc means data=cucumber n mean std ;
   var cucumbers;
   /* get the total number of transects */
   output out=weight n=samplesize;
run;
 
data cucumber;
   merge cucumber weight;
   retain samplingweight;
   /* we divide the shore length by 4 because each transect is 4 m wide */
   if samplesize > . then samplingweight = 51436/4 / samplesize;
run;
*---part003e;
   


/***** First check to see if any transects are missing quadrats *************/
 
ods document name=plot1(write);
*---part002b;
proc sgplot data=cucumber;
   title2 'plot the relationship between the cluster total and cluster size';
   scatter y=cucumbers x=area / datalabel=transect;  /* use the transect number as the plotting character */
run;
*---part002e;
ods document close;

 
/****************************************************************************/
/* First do a simple inflation estimator based on the cluster totals */

*---part004b;
proc surveymeans data=cucumber mean clm  sum clsum  cv ;
   /* N not specified as we ignore the fpc in this problem */
   /* mean clm   - find estimate of mean and confidence intervals   */
   /* sum  clsum - find estimate of grand total and confidence intervals */
   title2 'Simple inflation estimator using cluster totals';
   var cucumbers;
   weight samplingweight;
   ods output statistics=cucumberresultssimple;
run;
*---part004e;



/****************************************************************************/
/* Now for a formal cluster analysis - because the data is already summarized, this
   is equivalent to a simple ratio estimator */
/* You can't use the cluster analysis from Proc SurveyMeans because the raw data
   is not available */

*---part005b;
proc surveymeans data=cucumber   ratio clm ;
   /* the ratio clm keywords request a ratio estimator and a confidence interval. */
   title2 'Estimation using a ratio estimator';
   var cucumbers area;
   ratio cucumbers / area;
   ods output ratio=cucumberratio;   /* extract information so that total can be estimated */
run;

data cucumbertotal;
   /* compute estimates of the total */
   set cucumberratio;
   cv = stderr / ratio;  /* the relative standard error of the estimate */
   Est_total = ratio * 3769280;
   Se_total  = stderr* 3769280;
   UCL_total = uppercl*3769280;
   LCL_total = lowercl*3769280;
   format est_total se_total ucl_total lcl_total 7.1;
   format cv 7.2;
   format ratio stderr lowercl uppercl  7.3;
run;
*---part005e;

proc print data=cucumbertotal split='_';
   title3 'the estimated totals after expansion by the area';
   var Est_total Se_total LCL_total UCL_total;
run;



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


ods tagsets.mylatex file="cucumber-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=cucumber(obs=10);
run;
ods tagset.mylatex close;


goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='cucumber-SAS-prelim' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file="cucumber-SAS-simple.tex" (notop nobot) /* newfile=table */;
proc print data=cucumberresultssimple noobs label split=" ";
   var varname  mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean Sum LowerCLSum stderr upperCLmean stddev UpperCLsum 10.3;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file="cucumber-SAS-ratio.tex" (notop nobot) /* newfile=table */;
proc print data=cucumberratio noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="cucumber-SAS-total.tex" (notop nobot) /* newfile=table */;
proc print data=cucumbertotal split='_';
   var ratio stderr lowercl uppercl Est_total Se_total LCL_total UCL_total;
run;
ods tagsets.mylatex close;
