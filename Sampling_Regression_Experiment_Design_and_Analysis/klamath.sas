/* Klamath River data from Hirsch et al */
/* Total phosphorus from NASQAN site 115305.00 on Klamath River, near Klamath Ca.
   The data was extracted from the NASQAN database and modified to match Figure 1 of Hirsch et al.
   This is the data used by Hirsch et al. (1982) */

/* 2014-08-11 CJS Update for SAS 9.4 */
dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
footnote ' ';

options orientation=landscape;
ods pdf file='klamath-SAS.pdf' style=styles.printer;
 
title 'Klamath River Total Phosphorus';

*---part010b;
data klamath;
   infile 'klamath.csv' firstobs=2 dlm=',' dsd missover;
   input month y1972 y1973 y1974 y1975 y1976 y1977 y1978 y1979;
   array yearly(8) y1972 y1973 y1974 y1975 y1976 y1977 y1978 y1979;
   do i=1 to dim(yearly);
      phosphorus = yearly(i);
	  year       = i+ 1971;
	  year_month = year+(month-1)/12;
	  output;
   end;
   keep year month phosphorus year_month;
run;
*---part010e;

proc sort data=klamath; by year month; run;
proc print data=klamath (obs=10);
   title2 'part of raw data';
run;

proc tabulate data=klamath;
   title2 'summary of data';
   class year month;
   var  phosphorus;
   table month ALL , (year ALL)*phosphorus=' '*(n*f=3.0 mean='value'*f=5.2)/ rts=10;
run;

*proc tabulate data=klamath noseps;
*   title2 'summary of data';
*   class year month;
*   var  phosphorus;
*   table month , year*phosphorus*mean*f=5.2/ rts=10;
*run;

ods document name=prelimplot(write);
proc sgplot data=klamath;
   title2 'Preliminary data plot';
   scatter x=year_month y=phosphorus;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Total Phosphorus (mg/L)";
run;
ods document close;


/****************************************************************************************/
/********* Compute the empirically seasonally adjusted values and do a plot *******/
*---partdoSAb;
proc sort data=klamath; by month; run;
proc means data=klamath noprint;
   by month;
   var phosphorus;
   output out=month_medians median=med_phosphorus; 
run; 

proc print data=month_medians;
   title2 'Monthly medians';
run;

data sa_klamath;
   merge klamath  month_medians;
   by month;
   sa_phosphorus = phosphorus - med_phosphorus;
run;
*---partdoSAe;
 
proc print data=sa_klamath (obs=20);
   title2 'portion of seasonally adjusted data';
run;
 
proc tabulate data=sa_klamath noseps;
   title2 'Seasonally adjusted phosphorus data';
   class year month;
   var  sa_phosphorus;
   table month , year*sa_phosphorus=' '*mean=' '*f=5.2/ rts=10;
run;

ods document name=prelimplot_SA(write);
proc sgplot data=sa_klamath;
   title2 'Preliminary data plot after seasonal adjustment';
   scatter x=year_month y=sa_phosphorus;
   reg     x=year_month y=sa_phosphorus;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Seasonally adjusted Total Phosphorus (mg/L)";
run;
ods document close;

*---partregfitsab;
proc reg data=sa_klamath;
   title2 'fit regression line to seasonally adjusted data including outliers';
   model sa_phosphorus = year_month / dw dwprob clb clm;
   ods output OutputStatistics    =regfitSAOutputStatistics;
   ods output ParameterEstimates  =regfitSAParameterEstimates;
   ods output dwstatistic         =regfitSADWstatistic;
run;
*---partregfitsae;

*---partremoveoutliersb;
data sa_klamath_nooutliers;  /* remove all outliers */
   set sa_klamath;
   if sa_phosphorus > .20 then sa_phosphorus = .;
run;
*---partremoveoutlierse;

proc reg data=sa_klamath_nooutliers ;
   title2 'fit regression line to seasonally adjusted data without outliers';
   model sa_phosphorus = year_month / dw dwprob clb clm;
   ods output OutputStatistics    =regfitSANOOutputStatistics;
   ods output ParameterEstimates  =regfitSANOParameterEstimates;
   ods output dwstatistic         =regfitSANODWstatistic;
run;

ods document name=prelimplot_SANO(write);
proc sgplot data=sa_klamath_nooutliers;
   title2 'Preliminary data plot after seasonal adjustment without outliers';
   scatter x=year_month y=sa_phosphorus;
   reg     x=year_month y=sa_phosphorus;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Seasonally adjusted Total Phosphorus (mg/L)";
run;
ods document close;





/*************** Seasonal adjustment using ANCOVA methods ************/
ods document name=ancovaPrelimPlot;
ods graphics / reset attrpriority=none; 
proc sgplot data=klamath;
   title2 'Preliminary data plot with individual lines for each month after outliers removed';
   where phosphorus <= 0.20;
   scatter x=year_month y=phosphorus / group=month;
   reg     x=year_month y=phosphorus / group=month;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Seasonally adjusted Total Phosphorus (mg/L)";
run;
ods document close;


proc sort data=klamath; by month; run;

*---partglmfitnonpb;
ods graphics on;
proc glm data=klamath plots=all;
   title2 'fit the non-parallel slope model after outliers removed';
   where phosphorus <= .20;
   class month;
   model phosphorus = month year month*year / clparm solution;
   ods output ParameterEstimates =glmfitnonpParameterEstimates;
   ods output ModelAnova         =glmfitnonpModelAnova;
run;
*---partglmfitnonpe;

ods document name=glmfitp(write);
*---partglmfitpb;
ods graphics on;
proc glm data=klamath plots=all;;
   title2 'fit the parallel slope model';
   where phosphorus <= .20;
   class month;
   model phosphorus = month year  / predicted  clparm solution;
   estimate 'Common slope' year 1;
   ods output PredictedValues    =fitparallel;
   ods output ModelAnova         =glmfitpModelAnova;
   ods output ParameterEstimates =glmfitpParameterEstimates;
   id year month year_month;
run;
*---partglmfitpe;
ods document close;
 
proc sort data=fitparallel; by year month; run;
proc sort data=klamath;     by year month; run;
data fitparallel;
   merge fitparallel klamath;
   by year month;
run;
proc print data=fitparallel (obs=10);
   title3 'part of the fitted data';
run;

ods document name=glmfitpfittedplot(write);
ods graphics / reset attrpriority=none; 
proc sgplot data=fitparallel;
   title2 'Parallel slope model - outliers removed';
   where phosphorus <= 0.20;
   scatter x=year_month y=phosphorus / group=month;
   reg     x=year_month y=predicted  / group=month;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Seasonally adjusted Total Phosphorus (mg/L)";
run;
ods document close; 









*------------------------------------------------------------------------------------------;
/***************** use Cos and Sine functions to do the seasonal adjustment ***********/
 
*---partsinaddb;
/* add the sine and cosine terms */
data klamath_sin;
   set klamath;
   cos = cos(2*3.14159*year_month/1);
   sin = sin(2*3.14159*year_month/1);
   format year_month cos sin 7.2;
run;
*---partsinadde;

ods document name=sinfit(write);
*---partsinfitb;
ods graphics on;
proc reg data=klamath_sin plot=all;
   title2 'Fit model to cosine and sine functions';
   model phosphorus = year_month cos sin / dw dwprob p r;
   ods output OutputStatistics = cosfit1;
run;
*---partsinfite;
ods document close;

 
data cosfit1;
   merge cosfit1 klamath_sin;
run;
 
proc print data=cosfit1 (obs=10);
   title2 'selected data points from cos/sin fit to entire dataset';
run;

*---partsinNOb;
/* remove outliers, and refit */
data klamath_nooutliers;
   set klamath_sin;
   if phosphorus > .20 then phosphorus=.;
run;
*---partsinNOe;
 
ods document name=sinfitNO(write);
*---partsinfitNOb;
ods graphics on;
proc reg data=klamath_nooutliers plots=all;
   title2 'Fit model to cosine and sine functions - after outliers are removed';
   model phosphorus = year_month cos sin / dw dwprob p r;
   ods output OutputStatistics = cosfit2;
   ods output ParameterEstimates=sinfitNOparameterestimates;
   ods output fitstatistics     =sinfitNOfitstatistics;
   ods output dwstatistic       =sinfitNOdwstatistic;
run;
*---partsinfitNOe;
ods document close;

data cosfit2;
   merge cosfit2 klamath_nooutliers;
run;

proc print data=cosfit2 (obs=10);
   title2 'selected data points from cos/sin fit after outliers removed';
run;






/* try a robust regression on the original data */
/* A robust regression will downweight any outliers automatically */
ods graphics on;
proc robustreg data=klamath_sin plots=all;
   title2 'robust regression on all of the data';
   model phosphorus = year_month cos sin;
run;





/******************** Seasonal Kendall *****************************************/
/* We compute a seasonal Kendall by summing the individual
   Kendall computed for each month. Because SAS can't provide the
   actual test statistics, we "fake" this by adding together
   the actual z-values from the individual series. This won't be
   exact, but likely is close enough */

/* Seasonal Kendall starts by sorting by month. */
 
/* Next Kendalls Tau is computed for each month using the 
   data over the years */
ods listing close; run;
*---partSKb;
proc sort data=klamath; by month; run;
proc corr data=klamath kendall ;
   title2 'Get Kendall Tau for each month';
   by month; 
   var phosphorus year;
   ods output KendallCorr = month_kendall; 
run;
*---partSKe;
ods listing;
 
data month_kendall(rename=(year=kendall pyear=pvalue));
   set month_kendall;
   if variable='phosphorus';
   keep month year pyear;
run; 
 
proc print data=month_kendall noobs;
   title2 "Kendall's Tau for each month";
run;

/* Now to work backwards from the kendall value and the pvalue to get the
   estimated variance */
*---partSK2b;
data month_kendall2;
   /* Here is where we can't get the actual test statistics, and have to fake it */
   set month_kendall;
   z = probit(1-pvalue/2)* sign(kendall);;
   var = 1;
run;
*---partSK2e;
 
proc print data=month_kendall2 noobs; 
   sum z var;
run;

/* now to add up all the kendall coefficients and their variances */
proc means data=month_kendall2 noprint;
   var kendall var;
   output out=seasonal_kendall sum(z)=total_z sum(var)=total_var;
run;
 
data seasonal_kendall;
   set seasonal_kendall;
   z = total_z / sqrt(total_var);
   pvalue = 2*(1-probnorm(abs(z)));
   drop _type_ _freq_;
run;
 
proc print data=seasonal_kendall;
   title2 'Approximate Seasonal Kendall and estimated p-value';
run;


/* now to estimate the actual slopes using Sens formula */
proc sort data=klamath; by month;  /* must sort in advance */ 
proc iml;
   use klamath;
   read all var{year month phosphorus} into ymp where(phosphorus ^= .);
   by = {2};
   unique_rows = uniqueby( ymp, by, 1:nrow(ymp)); 
   unique_rows = unique_rows // (nrow(ymp)+1);
   print "Unique rows matrix" unique_rows;

   slopes = j(1,1,0);  /* create a vector to hold slopes with a dummy first value */
   do bygroup=1 to nrow(unique_rows)-1;
      do i= unique_rows[bygroup]  to unique_rows[bygroup+1]-1;
         do j=i+1 to unique_rows[bygroup+1]-1;
            if ymp[i,1] ^= ymp[j,1] then do;  /* ignore pairs where the x's are the same */
               slope = (ymp[j,3]-ymp[i,3])/(ymp[j,1]-ymp[i,1]);  /* slope from pairs of points */
               slopes = slopes // slope;  /* add slope to end of list */
            end;
         end;
      end;
   end;
   slopes = slopes[2:nrow(slopes)];  /* drop the initial dummy value */
   call sort( slopes, 1);  /* sort the slopes */
   print "Sorted slope values", slopes;

   /* figure out the appropriate percentiles to use */
   NN = nrow(slopes);
   n  = nrow(xy);
   estslope = median(slopes);
   lower=.;  /* no way to compute lower/upper bounds for confidence interval */
   upper=.;
   mattrib estslope format=7.4;
   mattrib lower    format=7.4;
   mattrib upper    format=7.4;
   print "Estimated slope and 95% confidence interval", estslope lower upper;
run;


/********** Seasonal adjustment with autocorrelation ********/
*---partSAdefmonthb;
data klamath;
   set klamath;
   month_start = (year-1972)*12 + month-1;
run;
*---partSAdefmonthe;

ods document name=SAAR(write);
*---partSAARb;
ods graphics on;
proc mixed data=klamath maxiter=200 maxfunc=1000 plots=all;;
   where phosphorus <= .20;  /* remove outliers */
   title2 'correlation among month modelled using ar(1) process';
   class month;
   model phosphorus = month year / solution ddfm =satterth;
   repeated    / subject=intercept  type=sp(pow)(month_start) ;
   estimate 'avg slope' year 1;
   ods output estimates    =SAARparameterestimates;
   ods output covparms     =SAARcovparms;
run;
*---partSAARe;
ods document close;


ods document name=SAIN(write);
*---partSAINb;
ods graphics on;
proc mixed data=klamath maxiter=200 maxfunc=1000 plots=all;
   where phosphorus <= .20;  /* remove outliers */
   title2 'correlation among month modelled using INDEPENDENT process';
   class month;
   model phosphorus = month year / solution ddfm =satterth;
   estimate 'avg slope' year 1;
   ods output estimates=SAINparameterestimates;
run;
*---partSAINe;
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

ods tagsets.mylatex file='klamath-SAS-010.tex' (notop nobot);
proc print data=klamath(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-prelimplot-SA' reset=index;
proc document name=prelimplot_SA;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='klamath-SAS-regfitSAparameterestimates.tex' (notop nobot);
proc print data=regfitSAParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-regfitSAdwstatistics.tex' (notop nobot);
proc print data=regfitSAdwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-regfitSAOutputStatistics.tex' (notop nobot);
proc print data=regfitSAOutputStatistics label split=" " noobs;
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-prelimplot-SANO' reset=index;
proc document name=prelimplot_SA;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='klamath-SAS-regfitSANOparameterestimates.tex' (notop nobot);
proc print data=regfitSANOParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-regfitSANOdwstatistics.tex' (notop nobot);
proc print data=regfitSANOdwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-regfitSANOOutputStatistics.tex' (notop nobot);
proc print data=regfitSANOOutputStatistics label split=" " noobs;
run;
ods tagsets.mylatex close;




*----------------------------------------------------------------;
* ANCOVA stuff ;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-ANCOVAprelimplot' reset=index;
proc document name=ancovaPrelimPLot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='klamath-SAS-glmfitnonpparameterestimates.tex' (notop nobot);
proc print data=glmfitnonpParameterEstimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-glmfitnonpmodelanova.tex' (notop nobot);
proc print data=glmfitnonpModelanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-glmfitpparameterestimates.tex' (notop nobot);
proc print data=glmfitpParameterEstimates(drop=dependent tvalue probt) noobs label split=' ';
   where lowcase(parameter)='year';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-glmfitpmodelanova.tex' (notop nobot);
proc print data=glmfitpModelanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-glmfitpfittedplot' reset=index;
proc document name=glmfitpfittedplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-glmfitpdiagplot' reset=index;
proc document name=glmfitp;
   replay  	\GLM#1\ANOVA#1\phosphorus#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;


*---------------------------------------------------------------------------;
* Sin/Cose fit stuff;

ods tagsets.mylatex file='klamath-SAS-sindata.tex' (notop nobot);
proc print data=klamath_sin(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-sinfitdiagplot' reset=index;
proc document name=sinfit;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\phosphorus#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='klamath-SAS-sinfitNOparameterestimates.tex' (notop nobot);
proc print data=sinfitNOparameterestimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-sinfitNOdwstatistic.tex' (notop nobot);
proc print data=sinfitNOdwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='klamath-SAS-sinfitNOdiagplot' reset=index;
proc document name=sinfitNO;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\phosphorus#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;

*-----------------------------------------------------------------------------------;
* Stuff for power function model;

ods tagsets.mylatex file='klamath-SAS-SAINparameterestimates.tex' (notop nobot);
proc print data=SAINparameterestimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-SAARcovparms.tex' (notop nobot);
proc print data=SAARcovparms label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-SAARparameterestimates.tex' (notop nobot);
proc print data=SAARparameterestimates label split=" " noobs;
run;
ods tagsets.mylatex close;




*-------------------------------------------------------------------------------;
* Seasonal Kendall stuff;

ods tagsets.mylatex file='klamath-SAS-monthkendall.tex' (notop nobot);
proc print data=month_kendall label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='klamath-SAS-monthkendall2.tex' (notop nobot);
proc print data=month_kendall2 label split=" " noobs;
   sum z var;
run;
ods tagsets.mylatex close;
